<Query Kind="Program" />

// This is a pretty naive implementation of Tic-Tac-Toe meant to
// demonstrate the minimax algorithm. Note that it takes awhile to
// start up - 30 seconds on a 2010 mac book air. Also note that the
// Ruby isn't all that great as Ruby, but I don't care as long as it
// illustrates the algorithm well.
//
// You can see that there's a hard-coded assumption that the AI is the
// X player, means that no human will ever be able to win. I'm positive
// that is only a small taste of what is to come, once robots take
// over the world.

void Main()
{
	var game = new Game();
	game.Turn();

}


public class GameState : IComparable
{
	public char CurrentPlayer { get; set; }
	public char[] Board { get; set; }
	public IList<GameState> Moves { get; private set; }

	public int Rank
	{
		get
		{
			return this.FinalStateRank() ?? this.IntermediateStateRank();
		}
	}

	public GameState(char currentPlayer, char[] board)
		: this(currentPlayer, board, new List<GameState>()) { }

	public GameState(char currentPlayer, char[] board, IList<GameState> moves)
	{
		this.CurrentPlayer = currentPlayer;
		this.Board = board;
		this.Moves = moves;
	}

	public GameState Clone()
	{
		return new GameState(this.CurrentPlayer, this.Board.Clone() as char[], this.Moves.Clone());
	}

	// This is only ever called when it's the AI's (the X player) turn
	public GameState NextMove()
	{
		return this.Moves.Max(m => m);
	}

	public char NextPlayer()
	{
		return (this.CurrentPlayer == 'X' ? 'O' : 'X');
	}

	public bool IsFinalState()
	{
		return (this.IsWinner() || this.IsDraw());
	}

	public int? FinalStateRank()
	{
		int? result = null;
		if (this.IsFinalState())
		{
			result = 0;
			if (this.IsWinner())
				result = (this.WhoWon() == 'X') ? 1 : -1;
		}

		return result;
	}

	public bool IsDraw()
	{
		return (!this.Board.Any(c => c == default(Char)) && !this.IsWinner());
	}

	public int IntermediateStateRank()
	{
		// Recursively calls the Rank function on children
		var result = 0;
		var ranks = this.Moves.Select(m => m.Rank);
		if (this.CurrentPlayer == 'X')
			result = ranks.Max();
		else
			result = ranks.Min();
		return result;
	}

	public bool IsWinner()
	{
		return (this.WhoWon() != default(Char));
	}

	public Char WhoWon()
	{
		char result = default(Char);
		var winningSequence = new[]
		{
			// horizontal wins
			(0,1,2),
			(3,4,5),
			(6,7,8),
			
			// vertical wins
			(0,3,6),
			(1,4,7),
			(2,5,8),
			
			// diagonal wins
			(0,4,8),
			(6,4,2)
		}.FirstOrDefault(p =>
			this.Board[p.Item1] == this.Board[p.Item2]
			&& this.Board[p.Item2] == this.Board[p.Item3]
			&& this.Board[p.Item1] != default(Char));

		if (winningSequence != default((int, int, int)))
			result = this.Board[winningSequence.Item1];

		return result;
	}

	public int CompareTo(object obj)
	{
		return this.Rank.CompareTo((obj as GameState).Rank);
	}
}

public class GameTree
{

	public GameState Generate()
	{
		var initialGameState = new GameState('X', new char[9]);
		GenerateMoves(initialGameState);
		return initialGameState;
	}

	public void GenerateMoves(GameState gameState)
	{
		var nextPlayer = gameState.NextPlayer();
		for (int i = 0; i < 9; i++)
		{
			if (gameState.Board[i] == default(Char))
			{
				// this is an unplayed position
				var nextBoard = gameState.Board.Clone() as char[];
				nextBoard[i] = gameState.CurrentPlayer;

				var nextGameState = new GameState(nextPlayer, nextBoard);
				gameState.Moves.Add(nextGameState);
				GenerateMoves(nextGameState);
			}
		}
	}
}

public class Game
{
	public GameState GameState { get; set; }
	public GameState InitialGameState { get; set; }

	public Game()
	{
		this.GameState = (new GameTree()).Generate();
		this.InitialGameState = this.GameState.Clone();
	}

	public void Turn()
	{
		if (this.GameState.IsFinalState())
		{
			DescribeFinalGameState();
			
			Console.WriteLine("Play again? y/n");
			var answer = Console.ReadLine();
			if (answer.Trim().ToLower()[0] == 'y')
			{
				this.GameState = this.InitialGameState;
				Turn();
			}
		}
		else
		{
			if (this.GameState.CurrentPlayer == 'X')
			{
				Console.WriteLine("\r\n===============");
				this.GameState = this.GameState.NextMove();
				Console.WriteLine("\r\nX's move:\r\n");
				RenderBoard();
				Turn();
			}
			else
			{
				GetHumanMove();
				Console.WriteLine("The result of your move:");
				RenderBoard();
				Console.WriteLine();
				Turn();
			}
		}
	}

	public void RenderBoard()
	{
		string output = string.Empty;

		for (int i = 0; i < 9; i++)
		{
			output += $"{this.GameState.Board[i].AsGridCell(i)} ";
			switch (i % 3)
			{
				case 2:
					if (i != 8)
						output += "\r\n-------------\r\n";
					break;
				default:
					output += "| ";
					break;
			}
		}

		Console.WriteLine(output);
	}

	//
	public void GetHumanMove()
	{
		GameState move = null;
		bool done = false;

		do
		{
			Console.Write("\r\nEnter square # to place your 'O' in: ");
			string positionText = Console.ReadLine();
			
			int position;
			if (Int32.TryParse(positionText, out position))
				move = this.GameState.Moves.SingleOrDefault(m => m.Board[position] == 'O');

			if (move != null)
			{
				this.GameState = move;
				done = true;
				Console.WriteLine(position);
			}
			else
				Console.WriteLine("That's not a valid move");

		} while (!done);
	}

	public void DescribeFinalGameState()
	{
		Console.WriteLine();
		if (this.GameState.IsDraw())
			Console.WriteLine("It was a draw!");
		else
			Console.WriteLine($"{this.GameState.WhoWon()} won!");
	}
}

public static class Extensions
{
	public static IList<GameState> Clone(this IList<GameState> gameStates)
	{
		return gameStates.Select(s => s.Clone()).ToList();
	}

	public static char AsGridCell(this char cell, int index)
	{
		var result = index.ToString()[0];
		if (cell != default(Char))
			result = cell;
		return result;
	}
}