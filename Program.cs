using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace ConsoleApp1
{
    // Enumerates the different types of tokens that can be identified by the lexer.
    public enum TokenType
    {
        Identifier,
        Number,
        Operator,
        Delimiter,
        Keyword,
        SpecialChar, 
        Comment    
    }

    // Interface representing a token.
    public interface IToken
    {
        TokenType Type { get; }
        string Value { get; }
    }

    // Interface representing a syntax node.
    public interface ISyntaxNode
    {
    }

    // Abstract class for syntax nodes, provides a method for printing the node.
    public abstract class SyntaxNode : ISyntaxNode
    {
        public abstract void Print(int indent = 0);
    }

    // Class representing a token with its type and value.
    public class Token : IToken
    {
        public TokenType Type { get; }
        public string Value { get; }

        public Token(TokenType type, string value)
        {
            Type = type;
            Value = value;
        }
    }

    // Exception class for syntax-related exceptions.
    public class SyntaxException : Exception
    {
        public SyntaxException(string message) : base(message) { }
    }

    // Class representing a syntax node for an identifier.
    public class IdentifierNode : SyntaxNode
    {
        public string Identifier { get; }

        public IdentifierNode(string identifier)
        {
            Identifier = identifier;
        }

        public override void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Identifier: {Identifier}");
        }
    }

    // Class representing a syntax node for a numeric value.
    public class NumberNode : SyntaxNode
    {
        public double Value { get; }

        public NumberNode(double value)
        {
            Value = value;
        }

        public override void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Number: {Value}");
        }
    }

    // Class representing a syntax node for a binary expression.
    public class BinaryExpressionNode : SyntaxNode
    {
        public SyntaxNode Left { get; }
        public SyntaxNode Right { get; }
        public TokenType Operator { get; }

        public BinaryExpressionNode(SyntaxNode left, SyntaxNode right, TokenType @operator)
        {
            Left = left;
            Right = right;
            Operator = @operator;
        }

        public override void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Binary Expression: {Operator}");
            Left.Print(indent + 2);
            Right.Print(indent + 2);
        }
    }

    // Class representing a syntax node for an assignment.
    public class AssignmentNode : SyntaxNode
    {
        public IdentifierNode Identifier { get; }
        public SyntaxNode Expression { get; }

        public AssignmentNode(IdentifierNode identifier, SyntaxNode expression)
        {
            Identifier = identifier;
            Expression = expression;
        }

        public override void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Assignment:");
            Identifier.Print(indent + 2);
            Expression.Print(indent + 2);
        }
    }

    // Class representing a syntax node for a declaration.
    public class DeclarationNode : SyntaxNode
    {
        public IdentifierNode Identifier { get; }
        public SyntaxNode Expression { get; }

        public DeclarationNode(IdentifierNode identifier, SyntaxNode expression)
        {
            Identifier = identifier;
            Expression = expression;
        }

        public override void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Declaration:");
            Identifier.Print(indent + 2);
            Expression?.Print(indent + 2);
        }
    }

    // Interface for the lexer, responsible for tokenizing the input.
    public interface ILexer
    {
        List<IToken> Tokenize();
    }

    // Class implementing the lexer interface.
    public class Lexer : ILexer
    {
        private readonly string input;
        private int position;

        // Dictionaries and sets for keywords, operators, and delimiters.
        private readonly Dictionary<string, TokenType> keywords = new Dictionary<string, TokenType>
        {
            {"int", TokenType.Keyword},
            {"double", TokenType.Keyword}
        };

        private readonly Dictionary<char, TokenType> operators = new Dictionary<char, TokenType>
        {
            {'+', TokenType.Operator},
            {'-', TokenType.Operator},
            {'*', TokenType.Operator},
            {'/', TokenType.Operator},
            {'=', TokenType.Operator}
        };

        private readonly HashSet<char> delimiters = new HashSet<char>
        {
            '(', ')', '{', '}', ',', ';'
        };

        // Constructor takes the input source code.
        public Lexer(string input)
        {
            this.input = input;
            position = 0;
        }

        // Tokenizes the input and returns a list of tokens.
        public List<IToken> Tokenize()
        {
            var tokens = new List<IToken>();

            // Loop through the input characters.
            while (position < input.Length)
            {
                char currentChar = input[position];

                // Check for letters to identify keywords or identifiers.
                if (char.IsLetter(currentChar))
                {
                    string identifier = ReadIdentifier();
                    if (keywords.ContainsKey(identifier))
                    {
                        tokens.Add(new Token(keywords[identifier], identifier));
                    }
                    else
                    {
                        tokens.Add(new Token(TokenType.Identifier, identifier));
                    }
                }
              
                else if (char.IsDigit(currentChar) || currentChar == '.')
                {
                    string number = ReadNumber();
                    tokens.Add(new Token(TokenType.Number, number));
                }
                
                else if (operators.ContainsKey(currentChar))
                {
                    tokens.Add(new Token(operators[currentChar], currentChar.ToString()));
                    position++;
                }
                
                else if (delimiters.Contains(currentChar))
                {
                    tokens.Add(new Token(TokenType.Delimiter, currentChar.ToString()));
                    position++;
                }
                
                else if (currentChar == '#')
                {
                    ReadComment();
                }
           
                else if (char.IsWhiteSpace(currentChar))
                {
                    position++;
                }
               
                else
                {
                    throw new SyntaxException("Invalid character: " + currentChar);
                }
            }

            return tokens;
        }

        
        private string ReadIdentifier()
        {
            int startPosition = position;
            while (position < input.Length && (char.IsLetterOrDigit(input[position]) || input[position] == '_'))
            {
                position++;
            }
            return input.Substring(startPosition, position - startPosition);
        }

      
        private string ReadNumber()
        {
            int startPosition = position;
            while (position < input.Length && (char.IsDigit(input[position]) || input[position] == '.'))
            {
                position++;
            }
            return input.Substring(startPosition, position - startPosition);
        }

       
        private void ReadComment()
        {
           
            while (position < input.Length && input[position] != '\n')
            {
                position++;
            }
        }
    }

   
    public interface IParser
    {
        SyntaxNode Parse();
    }

    public class Parser : IParser
    {
        private readonly List<IToken> tokens;
        private int position = 0;

     
        public Parser(List<IToken> tokens)
        {
            this.tokens = tokens;
        }

       
        public SyntaxNode Parse()
        {
            try
            {
                return ParseProgram();
            }
            catch (SyntaxException ex)
            {
                Console.Error.WriteLine("Syntax error: " + ex.Message);
                return null;
            }
        }

        // Parses a program, which consists of statements.
        private SyntaxNode ParseProgram()
        {
            return ParseStatement();
        }

       private SyntaxNode ParseStatement()
        {
            if (Match(TokenType.Keyword, "int") || Match(TokenType.Keyword, "double") || Match(TokenType.Keyword, "float"))
            {
                return ParseDeclaration();
            }
            else
            {
                return ParseExpression();
            }
        }

       
        private SyntaxNode ParseDeclaration()
        {
            var dataType = tokens[position].Value;
            Match(TokenType.Keyword);
            var identifier = new IdentifierNode(tokens[position].Value);
            Match(TokenType.Identifier);
            if (Match(TokenType.Operator, "="))
            {
                var expression = ParseExpression();
                return new AssignmentNode(identifier, expression);
            }
            else
            {
                return new DeclarationNode(identifier, null);
            }
        }

  
        private SyntaxNode ParseExpression()
        {
            var term = ParseTerm();
            return ParseExpressionPrime(term);
        }

        private SyntaxNode ParseExpressionPrime(SyntaxNode left)
        {
            while (Match(TokenType.Operator, "+") || Match(TokenType.Operator, "-"))
            {
                var @operator = tokens[position - 1].Type;
                var right = ParseTerm();
                left = new BinaryExpressionNode(left, right, @operator);
            }
            return left;
        }

        // Parses a term.
        private SyntaxNode ParseTerm()
        {
            var factor = ParseFactor();
            return ParseTermPrime(factor);
        }

       
        private SyntaxNode ParseTermPrime(SyntaxNode left)
        {
            if (Match(TokenType.Operator, "*") || Match(TokenType.Operator, "/"))
            {
                var @operator = tokens[position - 1].Type;
                var right = ParseFactor();
                var binaryExpressionNode = new BinaryExpressionNode(left, right, @operator);
                return ParseTermPrime(binaryExpressionNode);
            }
            return left;
        }

      
        private SyntaxNode ParseFactor()
        {
            if (Match(TokenType.Identifier))
            {
                return new IdentifierNode(tokens[position - 1].Value);
            }
            else if (Match(TokenType.Number))
            {
                return new NumberNode(double.Parse(tokens[position - 1].Value));
            }
            else if (Match(TokenType.Delimiter, "("))
            {
                var expression = ParseExpression();
                Match(TokenType.Delimiter, ")");
                return expression;
            }
            else
            {
                throw new SyntaxException("Invalid factor");
            }
        }

        
        private bool Match(TokenType type)
        {
            return Match(type, null);
        }

        
        private bool Match(TokenType type, string value)
        {
            if (position < tokens.Count && tokens[position].Type == type && (value == null || tokens[position].Value == value))
            {
                position++;
                return true;
            }
            return false;
        }
    }

    
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("Enter your input:");
            string sourceCode = Console.ReadLine();

            
            ILexer lexer = new Lexer(sourceCode);

           
            List<IToken> tokens = lexer.Tokenize();

           
            foreach (IToken token in tokens)
            {
                Console.WriteLine($"Type: {token.Type}, Value: {token.Value}");
            }

            
            IParser parser = new Parser(tokens);

            SyntaxNode syntaxTree = parser.Parse();

            syntaxTree?.Print();
        }
    }
}
