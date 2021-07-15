struct Node 
	label::Union{Any, Node} # Any could be replaced with type of variables
	next::Union{Node, Nothing}
end # https://inst.eecs.berkeley.edu/~cs61a/fa20/proj/scheme_stubbed/#extra-credit-2-pt

# Separate input into a list of tokens to pass to Parser (mutate string array tokens)
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
end # (define (fn a b c) (+ (- a b) c)) 

# Create abstract syntax tree 
function parser()

end # try catch

# Read AST and evaluate/print output
function eval()

end

function main()
	running = true
	tokens = String[]

	while running
		running = tokenizer(tokens)
		println("TOKENIZER: " * string(tokens))
	end
end

main()