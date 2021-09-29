struct Node 
	label::Union{Any, Node} # Any could be replaced with type of variables
	next::Union{Node, Nothing}
end # https://inst.eecs.berkeley.edu/~cs61a/fa20/proj/scheme_stubbed/#extra-credit-2-pt

"Check whether a node is a leaf node or a branch node."
function is_leaf(node) 
	return node.next == nothing
end

"Print abstract syntax tree of nodes."
function print_ast(node, depth=0, head::Bool=true)
	if typeof(node) == Node
		if typeof(node.label) <: String
			println("\t"^depth * node.label)
		end
		if typeof(node.label) == Node
			print_ast(node.label, depth, true)
		end
		if head
			depth+=1
		end
		if !is_leaf(node)
			print_ast(node.next, depth, false)
		end 
	end
end

function print_ast_alternate(node, depth=0)
	if typeof(node.label) <: String
		println("\t"^depth * node.label)
	end
	if typeof(node.label) == Node
		print_ast_alternate(node.label, depth+1)
		println("\t"^depth * "---")
	end
	if !is_leaf(node)
		print_ast_alternate(node.next, depth)
	end 
end

# Separate input into a list of tokens to pass to parser (mutate string array tokens)
# Return whether or not the program should end (user (exit)s)
syntax_operators = [' ', '(', ')']
function tokenizer(tokens) 
	input = readline(stdin)
	nexttoken = ""
	if input == "(exit)"
		return false
	end
	for inputpos = 1:length(input)
		chr = input[inputpos]
		#=
		if chr == '\'' || '`'
			nestcounter = 0
			while chr != ' ' && inputpos
				if 
			end
		end
		=#
		if chr in syntax_operators
			if nexttoken != ""
				push!(tokens, nexttoken)
			end
			if chr != ' '
				push!(tokens, string(chr))
			end
			nexttoken = ""
		else
			nexttoken *= chr
			if inputpos == length(input)
				push!(tokens, nexttoken)
			end
		end

	end
	return true 
end 

# Helper function for parser to find the next node given a "(" (")")
function find_next_node(tokens)
	depth = 1 # return when depth is 0. 1 from popped "("
	for t in 1:length(tokens) # TODO Reason it was broken before was lack of 1:
		if tokens[t] == ")"
			depth -= 1
		elseif tokens[t] == "("
			depth += 1
		end
		if depth == 0
			println("FNN1: " * string(t))
			return t-1
		end
	end
	println("FNN2: " * string(length(tokens))) # should get pos 15
	return length(tokens)
end

# Create abstract syntax tree 
function parser(tokens) 
	AST = Node(nothing, nothing)
	if tokens != []
		token = popfirst!(tokens)
	else
		return nothing
	end
	if token == "("
		return Node(parser(tokens), parser(tokens))#[find_next_node(tokens):end]))
	elseif token == ")"
		return nothing
	else
		if tokens == []
			return nothing
		else
			return Node(token, parser(tokens))
		end
	end
end 

struct Type
	type::String
	value
end
# Struct function can only descend from abstract type SchemeTypes
# Structs cannot descend from other structs

struct Frame
	parent::Union{Frame, Nothing}
	values::Dict{String, Type} # BuiltinProcedure()
end

function type_to_string(t::Type)
	return t.type * ": " * string(t.value)
end

function print_typed_ast(node, depth=0, head::Bool=true)
	if typeof(node) == Node
		if typeof(node.label) <: Type
			println("\t"^depth * type_to_string(node.label))
		end
		if typeof(node.label) == Node
			print_typed_ast(node.label, depth, true)
		end
		if head
			depth+=1
		end
		if !is_leaf(node)
			print_typed_ast(node.next, depth, false)
		end 
	end
end

ints = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] # regex [0-9]+ = r"[0-9]+"
floats = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.'] # regex [0-9]*\.[0-9]+ = r"[0-9]*\.[0-9]+"
#chrs = ['a', 'b', 'c', 'd', 'e', 'f'] TODO Use regex or filter(isletter, str) || don't detect chrs like this 
#chrs regex #\\. 
#string regex \"[^"].+\"[^"] = r"\"[^\"].+\"[^\"]" ... r"'[^'].+'[^']" ... r"\"[^\"]+\""
# match(r"\"[^\"]+\"", original).match != original or is nothing indicates it is not a precise match.
# Read AST and create new typed AST
function evaltypes(node)
	typing = Type("None", "none") 
	if typeof(node.label) == Node
		typing = Node(Type("Function", node.label.label), evaltypes(node.label.next))
	elseif typeof(node.label) <: String
		if length(node.label) >= 2 && node.label[1:2] == "#\\"
			if length(node.label) == 3
				typing = Type("Char", node.label) 
			else
				println("ERROR: Type Character cannot consist of multiple characters.")
			end
			# use try catch to see chr is valid
			# If failed, clear AST, send error, and try again.

		elseif length(node.label) >= 2 && node.label[1:2] == "#("
			typing = Type("Vector", node.label)# Interpret type
		elseif node.label[1] == '#' && length(node.label) == 2
			if (node.label[2] == 'f' || node.label[2] == 't')
				typing = Type("Boolean", node.label)
			else
				println("ERROR: Boolean must be either #t or #f")
			end
		elseif node.label[1] == '"' && node.label[end] == '"' && length(node.label) >= 2

			typing = Type("String", node.label)
		elseif node.label == '`'
			typing = Type("Expression", node.next.label) # NODE.NEXT INSTEAD OF LABEL
		elseif all((x) -> (x in ints), node.label)
			typing = Type("Int", parse(Int, node.label)) # TODO make all type evaluators parse value to Julia types.
		elseif all((x) -> (x in floats), node.label)
			typing = Type("Float", node.label)
		else
			typing = Type("Symbol", node.label)
		end
		# first check it starts with #(x), then check "", else number or symbol.
		#generate content of node.label
		# if for chr in node.label cr in ints1
	else
		println("ERROR: Node should be Symbol or Node.")
	end
	if typeof(node.next) == Node
		return Node(typing, evaltypes(node.next)) 
	else
		return Node(typing, nothing)
	end
end # (testfn (fn  " "" "test" #(1 2)) `(+ (- 1 2) 3))

# Read AST and evaluate/print output
# TODO syntax detection and function types (error handling)
function evalscm_(node, frame, args=[]) # (+ 1 2)
	returnval = nothing
	node == nothing ? (return nothing) : nothing
	if typeof(node.label) <: Node
		evalscm(node.next, frame) # For starting node, is nothing. 
		# May adversely affect functions
		return evalscm(node.label, frame)
		
	else
		if node.label.type == "Function"
			#return eval(node.label.value)(evalscm(node.next, frame))
			# FIND FUNCTION
			fn = nothing
			currframe = frame
			while fn == nothing && currframe != nothing
				fn = get(frame.values, node.label.value, nothing)
				currframe = frame.parent
			end
			if fn == nothing
				println("ERROR: Function Error, cannot find function or symbol.") # function not defined, rather.
				#break (cannot use in function)
			end
			# END FIND FUNCTION
			# TODO: Make new frames as you enter functions
			println("FN VALUE:" * string(fn.value))
			#println(evalscm(node.next, frame, args))
			#println(args) #why were these out here? if there is no node.next, it should call fn with no args.
			if typeof(node.next) <: Node
				evalscm(node.next, frame, args)
				println(args) # ARGS MESSING UP DUE TO SHARED ARGS ACROSS FUNCTIONS
				returnval = fn.value(args...)
				#return fn.value(args...) # Does not return schemetype
				#return eval(fn.value)(args...)
				#return eval(fn.value)(push!([], evalscm(node.next, frame)))
			else
				returnval = fn.value(nothing)
				#return eval(fn.value)(nothing) # ??? What was this?
			end
		elseif node.label.type == "Int"
			#returnval = parse(Int, node.label.value)
			returnval = node.label # Moved the parsing of Int type from string to int from here to the evaltypes function
		end
		if typeof(node.next) <: Node # ALL RETURNS SHOULD GO IN THIS SECTION
			push!(args, returnval)
			println("ARGS0: " * string(args))
			evalscm(node.next, frame, args)
			println("ARGS1: " * string(args))
			return returnval#args
			#return push!([returnval], evalscm(node.next, frame)) 
		else
			push!(args, returnval)
			println("ARGS2: " * string(args))
			return returnval#args
		end # how to make recursive function append to single array
	end
end # try catch # employ real eval() in Base
# (+ 1 2)     (+ 1 (+ 2 3) 4 (+ 5)) #//args are not Type. !!!!!

function evalscm(node, frame)
	# When there is a new function, open up new args stream. 
	# Pass as schemetypes and use special print functions for output.
	# TODO: Return is for output to print, and possibly filling args?
	returnval = nothing
	node == nothing ? (return nothing) : nothing
	if typeof(node.label) <: Node
		evalscm(node.next, frame) # For starting node, is nothing.  <- Whatever is there is not getting returned. Is it always nothing?
		# May adversely affect functions
		return evalscm(node.label, frame)
		
	else
		if node.label.type == "Function"
			# FIND FUNCTION
			fn = nothing
			currframe = frame
			while fn === nothing && currframe !== nothing
				fn = get(frame.values, node.label.value, nothing)
				currframe = frame.parent
			end
			if fn === nothing
				println("DEFINITION ERROR: Function or symbol is not defined.") # Theoretically Function and Symbol should be same type; just a key to lookup in the frames.
				#break (cannot use in function)
			end
			# END FIND FUNCTION

			# GENERATE ARGUMEENTS AND RETURN
			#=argstream = []
			if typeof(node.next) <: Node
				argstream = evalscm(node.next, frame) # doing way more assignment than I need to. argstream is almost completely done by lower levels, right?
				println(argstream) # ARGS MESSING UP DUE TO SHARED ARGS ACROSS FUNCTIONS
				returnval = fn.value(argstream...)
			else
				returnval = fn.value(nothing)
			end=#
			# nothing has no value next, but above already assumes it's not nothing.
			return fn.value(evalscm(node.next, frame)...) # fn.value(evalscm(node.next, frame)...) # (Iterates)
			# return or returnval? Find way to get node.next.next being heard instead of just node.next.label

		elseif node.label.type == "Int" # or [any] other type # TODO: Move string-to-type parsing from here to evaltypes for all types (e.g. Type("Char", 'c') not Type("Char", "\#c"))
			#returnval = parse(Int, node.label.value)
			returnval = node.label # Moved the parsing of Int type from string to int from here to the evaltypes function
		end
		# RETURNING 
		if typeof(node.next) <: Node # ALL RETURNS SHOULD GO IN THIS SECTION
			args = []
			push!(args, returnval)
			println("ARGS0: " * string(args))
			#[push!(args, arg) for arg in evalscm(node.next, frame)] #[] # This should be adding Int 4 to the args for +(+(1,2)4) but seems to be getting pased over.
			outpt = evalscm(node.next, frame)
			println("Extra-args: " * string(outpt))
			push!(args, outpt[1])
			println("ARGS1: " * string(args))
			return args#returnval
			#return push!([returnval], evalscm(node.next, frame)) 
		else
			#push!(args, returnval)
			#println("ARGS2: " * string(args))
			println("ARGS2: " * string(returnval) )
			return [returnval]#args
		end # how to make recursive function append to single array
	end
	#Node(Node(Type("Function", "+"), Node(Node(Type("Function", "+"), Node(Type("Int", 1), Node(Type("Int", 2), nothing))), Node(Type("Int", 4), nothing))), nothing)
	# Int 4 is node.next to the second Function +. For some reason, it's evaluating Int 4 then ignoring it in the addition for the first Function +.
end

function main()
	running = true # TODO delete these and replace with tokens returning its own Array{String}
	tokens = String[]
	#AST = Node[]
	AST = Node(nothing, nothing)
	typeAST = AST # TODO delete and handle typing by catching julia exceptions

	#nodetest = Node("define", Node(Node("fn", Node("a", Node("b", Node("c", nothing)))), Node(Node("+", Node(Node("-", Node("a", Node("b", nothing))),Node("c", nothing))), nothing)))
	#println("REFERENCE: ")
	#print_ast(nodetest)

	base = Dict( # TODO Make Function separate type that descends Type and handles input types
	"+" => Type("BuiltinFunction", (args...) -> (println("+" * string(args)); sum = 0; for arg in args; sum += arg.value; end; return Type("Int", sum))), # return Type Int val sum # TODO int/float adding with julia typechecks, DON'T STORE TYPES
	"-" => Type("BuiltinFunction", (args...) -> (diff = 0; length(args) > 1 ? (return Type("Int", args[1] - sum(args[2:end]))) : (return Type("Int", args[1]))))
	# Maybe BuiltinFunction(["Int"], (args...) -> ...) so it can tell what inputs are valid for each position, as well as how many. Not sure how it'd handle infinite.
	)
	globalframe = Frame(nothing, base)

	while running
		print("scm> ")
		running = tokenizer(tokens)
		println("TOKENIZER: " * string(tokens))
		println("PARSER:    ")
		AST = parser(tokens)
		println(AST)
		println("AST:       ")
		print_ast(AST)
		typedAST = evaltypes(AST)
		println(typedAST)
		println("Typed AST: ")
		print_typed_ast(typedAST)
		println(evalscm(typedAST, globalframe))
		
		# Test Cases:

		#(define (fn a b c) (+ (- a b) c)) 
		#((lambda (a b) (+ a b)) 1 2)
		#(let ((a 1) (b 2)) (+ a b))
		#(set "hello" "goodbye")
		#(testfn (fn #\c "hello" #f) (func (+ 1 2) #t))
		#(testfn (fn  " "" "test" #(1 2)) `(+ (- 1 2) 3))
		#(+ (+ 1 2) (+ 3 (+ 4 5)))
		#(+ (+ 1 2) 2 (+ 3 (+ 4 5)))
		#(+ 1 2)
		#(+ (+ 1 2) 4)
		#(+ 4 (+ 5 7))

	end
end

main()