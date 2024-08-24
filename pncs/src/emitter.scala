import panther._
import system.io._

object EmitSection {
    val Header = 0
    val Types = 1
    val Methods = 2
    val Declarations = 3
}

case class Emitter(syntaxTrees: Array[SyntaxTree], root: Symbol, file: string) {
    var sections = new Array[string](4)

    sections(EmitSection.Header) = ""
    sections(EmitSection.Types) = ""
    sections(EmitSection.Methods) = ""
    sections(EmitSection.Declarations) = ""

    var indent = ""
    var symbol_prefix = ""
    var container: Symbol = root
    var container_has_this: bool = false

    val checker = new Checker(root)
    val nl = "\n"

    def emit(): unit = {
        emit_symbol(root)

        val text =
            banner_comment("forward references") +
            sections(EmitSection.Header) + nl +
            banner_comment("types") +
            sections(EmitSection.Types) + nl +
            banner_comment("DECLARATIONS") +
            sections(EmitSection.Declarations) + nl +
            banner_comment("METHODS") +
            sections(EmitSection.Methods)

        File.write_all_text(file, text)
    }

    def banner_comment(word: string): string =
        "/**\n" +
        " *  " + word + "\n" +
        " */\n"

    def emit_symbol(symbol: Symbol): unit = {
        if (symbol.kind == SymbolKind.Root) {
            emit_root_symbol(symbol)
        } else if (symbol.kind == SymbolKind.Class) {
            emit_class_symbol(symbol)
        } else if (symbol.kind == SymbolKind.Method) {
            emit_method_symbol(symbol)
        } else if (symbol.kind == SymbolKind.Constructor) {
            emit_constructor_symbol(symbol)
        } else if (symbol.kind == SymbolKind.Field) {
            emit_field_symbol(symbol)
        } else if (symbol.kind == SymbolKind.Parameter) {
            emit_parameter_symbol(symbol)
        } else if (symbol.kind == SymbolKind.Local) {
            emit_local_symbol(symbol)
        } else {
            emit_panic("unknown symbol kind: " + string(symbol.kind))
        }
    }

    def emit_symbols(symbols: Array[Symbol]): unit = {
        val num_symbols = symbols.length
        for (i <- 0 to (num_symbols-1)) {
            emit_symbol(symbols(i))
        }
    }

    def emit_type_of_symbol(symbol: Symbol, section: int): unit =
        emit_type(checker.get_type_of_symbol(symbol), section)

    def emit_return_type_of_symbol(symbol: Symbol, section: int): unit =
        emit_type(checker.get_return_type_of_symbol(symbol), section)

    def emit_class_def(symbol: Symbol): unit = {
        emitln("struct " + symbol.name + " {", EmitSection.Types)
        val symbols = symbol.members.symbols()
        val num_symbols = symbols.length
        for (i <- 0 to (num_symbols-1)) {
            if (symbols(i).kind == SymbolKind.Field || symbols(i).kind == SymbolKind.Parameter) {
                emit("  ", EmitSection.Types) // indent
                emit_type(checker.get_type_of_symbol(symbols(i)), EmitSection.Types)
                emitln(" " + symbols(i).name + ";", EmitSection.Types)
            } else {

            }
        }

        emitln("};", EmitSection.Types)
        emitln(EmitSection.Types)
    }

    def emit_class_decl(symbol: Symbol): unit =
        emitln("typedef struct " + symbol.name + " " + symbol.name + ";", EmitSection.Header)


    def emit_class_symbol(symbol: Symbol): unit = {
        println("emitting class symbol " + symbol.name + "...")
        if (symbol.has_declaration()) {
            if (symbol.declaration().kind == DeclarationKind.Class) {
                emit_class_decl(symbol)
                emit_class_def(symbol)
            } else {
            }
        } else {
        }

        val prevContainer = container
        val prevHasThis = container_has_this
        container = symbol
        container_has_this = symbol.has_declaration() && symbol.declaration().kind == DeclarationKind.Class
        val save_prefix = symbol_prefix
        symbol_prefix = symbol_prefix + symbol.name + "_"

        emit_symbols(symbol.members.symbols())

        symbol_prefix = save_prefix
        container_has_this = prevHasThis
        container = prevContainer
    }

    def emit_method_decl(symbol: Symbol, ctor: bool, has_this: bool): unit = {
        println("emitting method decl " + symbol.name + "...")
        emit_comment("emitting method decl " + symbol.name, EmitSection.Declarations)
        SymbolTreePrinter.print_symbol(symbol)
        // emit return type.
        if (ctor) {
            emit(container.name, EmitSection.Declarations)
        } else {
            emit_return_type_of_symbol(symbol, EmitSection.Declarations)
        }

        // emit name
        emit(" " + symbol_prefix, EmitSection.Declarations)
        if (ctor) {
            emit("new", EmitSection.Declarations)
        } else {
            emit(symbol.name, EmitSection.Declarations)
        }

        // emit parameters
        emit(" (", EmitSection.Declarations)
        val symbols = symbol.members.symbols()
        val num_symbols = symbols.length
        for (i <- 0 to (num_symbols-1)) {
            emit_type_of_symbol(symbols(i), EmitSection.Declarations)
            emit(" ", EmitSection.Declarations)
            emit(symbols(i).name, EmitSection.Declarations)
            if(i < num_symbols - 1) {
                emit(", ", EmitSection.Declarations)
            } else {}
        }

        emitln(");", EmitSection.Declarations)
    }


    def emit_method_decl(symbol: Symbol, has_this: bool): unit =
        emit_method_decl(symbol, false, has_this)

    def emit_constructor_symbol(symbol: Symbol): unit =
        emit_method_decl(symbol, true, false)

    def emit_field_symbol(symbol: Symbol): unit = {
        // emit_panic("emit_field_symbol: " + symbol.name)
    }

    def emit_local_symbol(symbol: Symbol): unit = {
        emit_panic("emit_local_symbol: " + symbol.name)
    }

    def emit_method_symbol(symbol: Symbol): unit = {
        // emit_method_decl(symbol, container_has_this)
        // val symbol_name = symbol_prefix + symbol.name

        // emit("/* return type */ ", EmitSection.Methods)
        // emit(symbol_name, EmitSection.Methods)
        // emitln(";", EmitSection.Methods)
    }

    def emit_parameter_symbol(symbol: Symbol): unit = {
        // emit(symbol.name, EmitSection.Declarations)
        // emit_panic("emit_parameter_symbol: " + symbol.name)
    }

    def emit_root_symbol(symbol: Symbol): unit = {
        val symbols = symbol.members.symbols()
        val num_symbols = symbols.length
        emit_comment("emitting root with " + string(num_symbols) + " symbols", EmitSection.Declarations)
        emitln(EmitSection.Declarations)
        emit_symbols(symbols)
    }

    def emit_comment(comment: string, section: int): unit = emitln("// " + comment, section)

    def emitln(value: string, section: int): unit = emit(value + "\n", section)

    def emitln(section: int): unit = sections(section) = sections(section) + "\n"

    def emit(value: string, section: int): unit = sections(section) = sections(section) + indent + value

    def emit_type(typ: Type, section: int): unit = {
        if (typ.kind == TypeKind.Primitive) {
            emit(typ.primitive.get.name, section)
        } else if (typ.kind == TypeKind.TypeConstructor) {
            emit(typ.typeConstructor.get.name, section)
        } else if (typ.kind == TypeKind.Array) {
            emit("array_", section)
            emit_type(typ.array.get.inner, section)
        } else if (typ.kind == TypeKind.Option) {
            emit("option_", section)
            emit_type(typ.option.get.inner, section)
        } else if (typ.kind == TypeKind.Unresolved) {
            // TODO: figure out how to resolve types automatically
            emit("unresolved", section)
        } else {
            emit_panic("emit_type: " + string(typ.kind))
        }
    }

    def emit_panic(value: string): unit = {
        println("--------------------------------------------------")
        println("-                   HEADER                       -")
        println("--------------------------------------------------")
        println(sections(EmitSection.Header))
        println("--------------------------------------------------")
        println("-                    TYPES                       -")
        println("--------------------------------------------------")
        println(sections(EmitSection.Types))
        println("--------------------------------------------------")
        println("-                 DECLARATIONS                   -")
        println("--------------------------------------------------")
        println(sections(EmitSection.Declarations))
        println("--------------------------------------------------")
        println("-                   METHODS                      -")
        println("--------------------------------------------------")
        println(sections(EmitSection.Methods))

        panic(value + ". symbol_prefix: " + symbol_prefix)
    }
}