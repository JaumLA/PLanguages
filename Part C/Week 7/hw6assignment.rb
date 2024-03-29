# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # instead of the pieces being randomly (and uniformly) chosen from the 7 classic pieces, the pieces are randomly (and uniformly) chosen from 10 pieces. They are the classic 7 and these 3: 5-long, utah, short-L.
  #The initial rotation for each piece is also chosen randomly.
  All_My_Pieces = All_Pieces + [
    [[[0, 0],[-1, 0], [1, 0], [2, 0], [-2,0]], # 5-long
     [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]],
    rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1,-1]]), # utah
    rotations([[0, 0], [1, 0], [0, 1]]) # short-L
  ]
  # your enhancements here
  
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
  
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end
  
  
    def next_piece
    #if cheat is possible, the next piece is a 1x1 block
    if @cheat
      @current_block = MyPiece.new([[[0, 0]]], self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end
  
    #  If the score is less than 100, nothing happens. Else the player loses 100 points (cheating costs you) and the next piece is a 1x1 block.
  def cheat
    if @score >= 100 and !@cheat_count
      @cheat = true
      @score -= 100
    end
  end
  
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    i = @current_block.num_blocks
    (0..i).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
end

class MyTetris < Tetris
  # your enhancements here
  
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super

#the player can press the ’u’ key to make the piece that is falling rotate 180 degrees.
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})

#
    @root.bind('c', proc {@board.cheat})
  end
  
end
