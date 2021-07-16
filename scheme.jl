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

struct Type
	type::String
	value
end

struct Frame
	parent::Union{Frame, Nothing}
	values::Dict{String, Type} 
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
	for t in length(tokens)
		if tokens[t] == ")"
			depth -= 1
		elseif tokens[t] == "("
			depth += 1
		end
		if depth == 0
			return t-1
		end
	end
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
	#=
	if ) return nothing (actual nothing)
	if ( add to AST label
	else return Node(token, parser(tokens))
	=#
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

# Read AST and evaluate/print output
function eval()

end # try catch

function main()
	running = true
	tokens = String[]
	#AST = Node[]
	AST = Node(nothing, nothing)

	nodetest = Node("define", Node(Node("fn", Node("a", Node("b", Node("c", nothing)))), Node(Node("+", Node(Node("-", Node("a", Node("b", nothing))),Node("c", nothing))), nothing)))
	println("REFERENCE: ")
	print_ast(nodetest)

	while running
		running = tokenizer(tokens)
		println("TOKENIZER: " * string(tokens))
		println("PARSER:    ")
		AST = parser(tokens)
		println(AST)
		println("AST:       ")
		print_ast(AST)

		#(define (fn a b c) (+ (- a b) c)) 
		#((lambda (a b) (+ a b)) 1 2)
		#(let ((a 1) (b 2)) (+ a b))

	end
end

main()