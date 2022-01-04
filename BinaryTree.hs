
data Arbol a = H a
  | N a (Arbol a)(Arbol a)
  deriving (Show, Eq)

nHojas :: Arbol a -> Int
nHojas (H _) = 1
nHojas (N x i d) = nHojas i + nHojas d

nNodos :: Arbol a -> Int
nNodos (H _) = 1
nNodos (N x i d) = 1 + nNodos i + nNodos d

profundidad :: Arbol a -> Int
profundidad (H _) = 0
profundidad (N x i d) = 1 + max (profundidad i) (profundidad d)

-- Regresa el orden del arbol de izquierda a derecha
preorden :: Arbol a -> [a]
preorden (H x) = [x]
preorden (N x i d) = x : (preorden i ++ preorden d)

-- Primero recorre el subarbol izquierdo, luego el derecho y por ultimo la raiz
posorden :: Arbol a -> [a]
posorden (H x) = [x]
posorden (N x i d) = posorden i ++ posorden d ++ [x]

-- Primero la raiz, luego el subarbol izquierdo y por ultimo el derecho, sin usar (++)
preordenIt :: Arbol a -> [a]
preordenIt x = preordenItAux x []
  where preordenItAux (H x) xs     = x:xs
        preordenItAux (N x i d) xs =
          x : preordenItAux i (preordenItAux d xs)

-- Funcion espejo de arbol recibido
espejo :: Arbol a -> Arbol a
espejo (H x) = H x
espejo (N x i d) = N x (espejo d) (espejo i)

takeArbol :: Int -> Arbol a -> Arbol a
takeArbol _ (H x)     = H x
takeArbol 0 (N x i d) = H x
takeArbol n (N x i d) =
  N x(takeArbol (n-1) i) (takeArbol (n-1) d)

{--
    9
   / \
   3  7
  / \
  2  4
--}
