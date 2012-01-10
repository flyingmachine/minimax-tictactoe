require 'rspec'
require 'tictactoe'

def ar(size, content = nil)
  Array.new(size, content)
end

describe GameState do
  describe "#winner" do
    it "should report a win when all spaces in a row have the same mark" do
      board = ar(3, 'X')
      game_state = GameState.new("X", board)
      game_state.winner.should == 'X'
    end
  end
end
