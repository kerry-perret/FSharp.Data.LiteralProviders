module internal FSharp.Data.LiteralProviders.DesignTime.BinaryFileProvider

open System.IO
open System.Reflection
open System.Text
open ProviderImplementation.ProvidedTypes

let private addFileMembers (ty: ProvidedTypeDefinition) (path: string) (name: string) =
    ty.AddMembersDelayed(fun () ->
        let byteContent = File.ReadAllBytes path
        [ yield ProvidedField.Literal("Path", typeof<string>, path) :> _
          yield ProvidedField.Literal("Name", typeof<string>, name) :> _
          yield ProvidedField.Read("Bytes", typeof<byte array>, (fun _ -> <@@ byteContent @@>), isStatic = true)
        ] : list<MemberInfo>)

let createFile asm ns baseDir =
    let createForFile (path: string) =
        let name = Path.GetFileName path
        let ty = ProvidedTypeDefinition(name, None)
        addFileMembers ty path name
        ty

    let rec createForDir (path: string) (isRoot: bool) =
        let ty =
            if isRoot
            then ProvidedTypeDefinition(asm, ns, "TextFile", None)
            else ProvidedTypeDefinition(Path.GetFileName path, None)
        ty.AddMembersDelayed(fun () ->
            [ for f in Directory.GetFiles(path) do yield createForFile f
              for d in Directory.GetDirectories(path) do yield createForDir d false ])
        ty

    createForDir baseDir true

let addFileOrDefault asm ns baseDir (ty: ProvidedTypeDefinition) =
    ty.DefineStaticParameters(
        [ProvidedStaticParameter("Path", typeof<string>); ProvidedStaticParameter("DefaultValue", typeof<array byte>, "")],
        fun tyName args ->
            let ty = ProvidedTypeDefinition(asm, ns, tyName, None)
            let path = Path.Combine(baseDir, args.[0] :?> string)
            let name = Path.GetFileName path
            let exists = File.Exists(path)
            ProvidedField.Literal("Exists", typeof<bool>, exists) |> ty.AddMember
            if exists then
                addFileMembers ty path name
            else
                ty.AddMembers(
                    [ ProvidedField.Literal("Path", typeof<string>, path)
                      ProvidedField.Literal("Name", typeof<string>, name)
                      ProvidedField.Literal("Bytes", typeof<byte array>, args.[1]) ])
            ty)
    ty

let create asm ns baseDir =
    createFile asm ns baseDir
    |> addFileOrDefault asm ns baseDir
