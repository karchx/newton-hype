module Numbers where

data Entidade = A | B | C | D | E | F | G 
		          | H | I | J | K | L | M | N
							| O | P | Q | R | S | T | U
							| V | W | X | Y | Z | Inespecifico
		deriving(Eq, Bounded, Enum)

instance Show Entidade where
	show A = "A"; show B = "B"; show C = "C";
	show D = "D"; show E = "E"; show F = "F";
	show G = "G"; show H = "H"; show I = "I";
	show J = "J"; show K = "K"; show L = "L";
	show M = "M"; show N = "N"; show O = "O";
	show P = "P"; show Q = "Q"; show R = "R";
	show S = "S"; show T = "T"; show U = "U";
	show V = "V"; show W = "W"; show X = "X";
	show Y = "Y"; show Z = "Z"; show Inespecifico = "*";

entidades :: [Entidade]
entidades = [minBound..maxBound]
