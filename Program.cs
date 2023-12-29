using System;
using System.Collections.Generic;

namespace ConsoleApp1
{
    public enum TokenType
    {
        Identifier,
        Number,
        Operator,
        Delimiter,
        Keyword
    }

    public interface IToken
    {
        TokenType Type { get; }
        string Value { get; }
    }

    public interface ISyntaxNode
    {
        void Print(int indent = 0);
    }

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

    public class SyntaxException : Exception
    {
        public SyntaxException(string message) : base(message) { }
    }

    public class IdentifierNode : ISyntaxNode
    {
        public string Identifier { get; }

        public IdentifierNode(string identifier)
        {
            Identifier = identifier;
        }

        public void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Identifier: {Identifier}");
        }
    }

    public class NumberNode : ISyntaxNode
    {
        public int Number { get; }

        public NumberNode(int number)
        {
            Number = number;
        }

        public void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Number: {Number}");
        }
    }

    public class BinaryExpressionNode : ISyntaxNode
    {
        public ISyntaxNode Left { get; }
        public ISyntaxNode Right { get; }
        public TokenType Operator { get; }

        public BinaryExpressionNode(ISyntaxNode left, ISyntaxNode right, TokenType @operator)
        {
            Left = left;
            Right = right;
            Operator = @operator;
        }

        public void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Binary Expression: {Operator}");
            Left.Print(indent + 2);
            Right.Print(indent + 2);
        }
    }

    public class AssignmentNode : ISyntaxNode
    {
        public IdentifierNode Identifier { get; }
        public ISyntaxNode Expression { get; }

        public AssignmentNode(IdentifierNode identifier, ISyntaxNode expression)
        {
            Identifier = identifier;
            Expression = expression;
        }

        public void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Assignment:");
            Identifier.Print(indent + 2);
            Expression.Print(indent + 2);
        }
    }

    public class DeclarationNode : ISyntaxNode
    {
        public IdentifierNode Identifier { get; }
        public ISyntaxNode Expression { get; }

        public DeclarationNode(IdentifierNode identifier, ISyntaxNode expression)
        {
            Identifier = identifier;
            Expression = expression;
        }

        public void Print(int indent = 0)
        {
            Console.WriteLine($"{new string(' ', indent)}Declaration:");
            Identifier.Print(indent + 2);
            Expression.Print(indent + 2);
        }
    }

    public interface ILexer
    {
        List<IToken> Tokenize();
    }

    public class Lexer : ILexer
    {
        private readonly string input;
        private int position;

        private readonly Dictionary<string, TokenType> keywords = new Dictionary<string, TokenType>
        {
            {"int", TokenType.Keyword}
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

        public Lexer(string input)
        {
            this.input = input;
            position = 0;
        }

        public List<IToken> Tokenize()
        {
            var tokens = new List<IToken>();

            while (position < input.Length)
            {
                char currentChar = input[position];

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
                else if (char.IsDigit(currentChar))
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
            while (position < input.Length && char.IsDigit(input[position]))
            {
                position++;
            }
            return input.Substring(startPosition, position - startPosition);
        }
    }

    public interface IParser
    {
        ISyntaxNode Parse();
    }

    public class Parser : IParser
    {
        private readonly List<IToken> tokens;
        private int position = 0;

        public Parser(List<IToken> tokens)
        {
            this.tokens = tokens;
        }

        public ISyntaxNode Parse()
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

        private ISyntaxNode ParseProgram()
        {
            return ParseStatement();
        }

        private ISyntaxNode ParseStatement()
        {
            if (Match(TokenType.Keyword, "int"))
            {
                return ParseDeclaration();
            }
            else
            {
                return ParseExpression();
            }
        }

        private ISyntaxNode ParseDeclaration()
        {
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

        private ISyntaxNode ParseExpression()
        {
            var term = ParseTerm();
            return ParseExpressionPrime(term);
        }

        private ISyntaxNode ParseExpressionPrime(ISyntaxNode left)
        {
            if (Match(TokenType.Operator, "+") || Match(TokenType.Operator, "-"))
            {
                var @operator = tokens[position - 1].Type;
                var right = ParseTerm();
                var binaryExpressionNode = new BinaryExpressionNode(left, right, @operator);
                return ParseExpressionPrime(binaryExpressionNode);
            }
            return left;
        }

        private ISyntaxNode ParseTerm()
        {
            var factor = ParseFactor();
            return ParseTermPrime(factor);
        }

        private ISyntaxNode ParseTermPrime(ISyntaxNode left)
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

        private ISyntaxNode ParseFactor()
        {
            if (Match(TokenType.Identifier))
            {
                return new IdentifierNode(tokens[position - 1].Value);
            }
            else if (Match(TokenType.Number))
            {
                return new NumberNode(int.Parse(tokens[position - 1].Value));
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
            string sourceCode = "int x = 10 + 20;";
            ILexer lexer = new Lexer(sourceCode);
            List<IToken> tokens = lexer.Tokenize();

            foreach (IToken token in tokens)
            {
                Console.WriteLine($"Type: {token.Type}, Value: {token.Value}");
            }

            // Create a parser
            IParser parser = new Parser(tokens);

            // Parse the source code
            ISyntaxNode syntaxTree = parser.Parse();

            // Print the syntax tree
            syntaxTree?.Print();
        }
    }
}
