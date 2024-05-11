-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:units"  ##############
-- #############################################################################

import Test.Hspec

import Board
    ( buildBoard,
      line,
      validateFEN,
      Board,
      Cell(Empty, Piece),
      Player(Black, White),
      Pos(Pos) )
import Ploy ( gameFinished, isValidMove, listMoves, Move(Move), possibleMoves )

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testLine
    testGameFinished
    testIsValidMove
    testPossibleMoves
    testListMoves

sampleBoard :: Board
sampleBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

testValidateFEN :: Spec
testValidateFEN = describe "Module Board: validateFEN ..." $ do
        it "fen has not 9 rows" $ do
            validateFEN ",,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)

        it "fen has not 8 columns" $ do
            validateFEN ",,,,,,,,,/,,,,,,,,/,,,,,,,,,/,,,,,,,,,/,,,,,,,,,/,,,,,,,,,/,,,,,,,,,/,,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)

        it "fen has only black and white pieces" $ do
            validateFEN  ",a84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,w16,w84,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,"
 `shouldBe` (False :: Bool)

        it "fen has to have rows" $ do
            validateFEN ",,,,,,,," `shouldBe` (False :: Bool)


testBuildBoard :: Spec
testBuildBoard = describe "Module Board: buildBoard ..." $ do
        it "build empty board" $ do
            buildBoard ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

        it "build start position" $ do
            buildBoard ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69," `shouldBe` [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

testLine :: Spec
testLine = describe "Module Board: line ..." $ do
        it "start is target" $ do
            line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos])

        it "a line" $ do
            line (Pos 'a' 1) (Pos 'i' 9) `shouldBe` ([Pos {col = 'a', row = 1},Pos {col = 'b', row = 2},Pos {col = 'c', row = 3},Pos {col = 'd', row = 4},Pos {col = 'e', row = 5},Pos {col = 'f', row = 6},Pos {col = 'g', row = 7},Pos {col = 'h', row = 8},Pos {col = 'i', row = 9}])

testGameFinished :: Spec
testGameFinished = describe "Module Game: gameFinished ..." $ do
        it "start board not finished" $ do
            gameFinished sampleBoard `shouldBe` (True :: Bool)

        it "game is finished" $ do
            gameFinished buildBoard ",,,,,,,,/,,,,b3,,,,/,,,,,,,,/,,,,w170,,,,/,,,,,,,,/,,,,b1,,b1,,/,,,,b1,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (True :: Bool)

        it "game is finished" $ do
            gameFinished buildBoard ",,,,,,,,/,,,,b3,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,b1,,b1,,/,,,,b1,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (True :: Bool)

        it "game is finished" $ do
            gameFinished buildBoard ",,,,,,,,/,,,,w3,,,,/,,,,,,,,/,,,,b170,,,,/,,,w170,,,,,/,,,,w1,,w1,,/,,,,w1,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (True :: Bool)

        it "game is not finished" $ do
            gameFinished buildBoard ",,,,,,,,/,,,,b3,,,,/,,,,,w170,,,/,,,b85,,,,,/,,,,w1,,,,/,,,,b1,,b1,,/,,,,b1,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)

        it "game is not finished" $ do
            gameFinished buildBoard ",,,,,,,,/,,,,b3,,,,/,,,,,w85,,,/,,,b85,,,,,/,,,,w1,,,,/,,,,b1,,b1,,/,,,,b1,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)



testIsValidMove :: Spec
testIsValidMove = describe "Module Game: isValidMove ..." $ do
        it "rotation by 1 is always possible" $ do
            isValidMove sampleBoard (Move (Pos 'c' 1) (Pos 'c' 1) 1) `shouldBe` (True :: Bool)

        it "shield can move and turn" $ do
            isValidMove b(buildBoard dene1) (Move (Pos 'd' 3) (Pos 'd' 4) 5) `shouldBe` (True :: Bool)

        it "beside shield move and turn is invalid" $ do
            isValidMove (buildBoard dene1) (Move (Pos 'c' 2) (Pos 'c' 3) 5) `shouldBe` (False :: Bool)
        
        it "is not horizontal vertical or diagonal" $ do
            isValidMove (buildBoard dene1) (Move (Pos 'd' 2) (Pos 'c' 4) 0) `shouldBe` (False :: Bool)

        it "cannot move more then it can" $ do
            isValidMove (buildBoard dene1) (Move (Pos 'h' 1) (Pos 'h' 5) 0) `shouldBe` (False :: Bool)

        it "cannot move that way" $ do
            isValidMove (buildBoard dene1) (Move (Pos 'd' 3) (Pos 'c' 4) 0) `shouldBe` (False :: Bool)

        it "cannot turn more then 7" $ do
            isValidMove (buildBoard dene1) (Move (Pos 'd' 3) (Pos 'd' 3) 8) `shouldBe` (False :: Bool)






testPossibleMoves :: Spec
testPossibleMoves = describe "Module Game: possibleMoves ..." $ do
        it "move shield one step" $ do
            possibleMoves (Pos 'd' 3) (Piece White 1) `shouldContain` ([Move (Pos 'd' 3) (Pos 'd' 4) 0] :: [Move])

        it "move probe two steps" $ do
            possibleMoves (Pos 'c' 8) (Piece White 24) `shouldContain` ([Move (Pos 'c' 8) (Pos 'c' 6) 0] :: [Move])

        it "probe only turning" $ do
            possibleMoves (Pos 'c' 8) (Piece White 24) `shouldContain` ([Move (Pos 'c' 8) (Pos 'c' 8) 6] :: [Move])


testListMoves :: Spec
testListMoves = describe "Module Game: listMoves ..." $ do
        it "game finished" $ do
            listMoves sampleBoard Black `shouldBe` ([] :: [Move])
