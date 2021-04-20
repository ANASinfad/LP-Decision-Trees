
data Mushroom = Mush Char String  
data DecisionTree = Node String [DecisionTree] | Fulla (String,String) --El primer String de la Fulla es el nombre del tipo de la columna y el segundo es para el poisonous o el edible

--El programa Principal
main :: IO ()
main = do
    contents <- readFile "agaricus-lepiota.data" 
    let input = (lines contents)
    let listOfMushrooms  = map lineToMushroom  input
    let allAttributes = foldl generateAttributes  []  listOfMushrooms 
    let listOfNames = [("cap-shape",[('b',"bell"),('c',"conical"),('x',"convex"),('f',"flat"),('k',"knobbed"),('s',"sunken")]),("cap-surface",[('f',"fibrous"),('g',"grooves"),('y',"scaly"),('s',"smooth")]),("cap-color",[('n',"brown"),('b',"buff"),('c',"cinnamon"),('g',"gray"),('r',"green"),('p',"pink"),('u',"purple"),('e',"red"),('w',"white"),('y',"yellow")]),("bruises?",[('t',"bruises"),('f',"no")]),("odor",[('a',"almond"),('l',"anise"),('c',"creosote"),('y',"fishy"),('f',"foul"),('m',"musty"),('n',"none"),('p',"pungent"),('s',"spicy")]),("gill-attachment",[('a',"attached"),('d',"descending"),('f',"free"),('n',"notched")]),("gill-spacing",[('c',"close"),('w',"crowded"),('d',"distant")]),("gill-size",[('b',"broad"),('n',"narrow")]),("gill-color",[('k',"black"),('n',"brown"),('b',"buff"),('h',"chocolate"),('g',"gray"),('r',"green"),('o',"orange"),('p',"pink"),('u',"purple"),('e',"red"),('w',"white"),('y',"yellow")]),("stalk-shape",[('e',"enlarging"),('t',"tapering")]),("stalk-root",[('b',"bulbous"),('c',"club"),('u',"cup"),('e',"equal"),('z',"rhizomorphs"),('r',"rooted"),('?',"missing")]),("stalk-surface-above-ring",[('f',"fibrous"),('y',"scaly"),('k',"silky"),('s',"smooth")]),("stalk-surface-below-ring",[('f',"fibrous"),('y',"scaly"),('k',"silky"),('s',"smooth")]),("stalk-color-above-ring",[('n',"brown"),('b',"buff"),('c',"cinnamon"),('g',"gray"),('o',"orange"),('p',"pink"),('e',"red"),('w',"white"),('y',"yellow")]),("stalk-color-below-ring",[('n',"brown"),('b',"buff"),('c',"cinnamon"),('g',"gray"),('o',"orange"),('p',"pink"),('e',"red"),('w',"white"),('y',"yellow")]),("veil-type",[('p',"partial"),('u',"universal")]),("veil-color",[('n',"brown"),('o',"orange"),('w',"white"),('y',"yellow")]),("ring-number",[('n',"none"),('o',"one"),('t',"two")]),("ring-type",[('c',"cobwebby"),('e',"evanescent"),('f',"flaring"),('l',"large"),('n',"none"),('p',"pendant"),('s',"sheathing"),('z',"zone")]),("spore-print-color",[('k',"black"),('n',"brown"),('b',"buff"),('h',"chocolate"),('r',"green"),('o',"orange"),('u',"purple"),('w',"white"),('y',"yellow")]),("population",[('a',"abundant"),('c',"clustered"),('n',"numerous"),('s',"scattered"),('v',"several"),('y',"solitary")]),("habitat",[('g',"grasses"),('l',"leaves"),('m',"meadows"),('p',"paths"),('u',"urban"),('w',"waste"),('d',"woods")])]
    let treeList = createDesicionTree allAttributes [] listOfNames
    let tree = build treeList listOfNames

    printTree tree 0
    userInteraction tree
    return ()


--Esta función recibe una linea de la entrada y la convierte en el tipo Muchroom sin comas. Ejemplo (p xsntpfcnkeesswwpwopksu) donde p es la clase del Mushroom que puede ser
--poisonous(p) o edible (e) y el string (xsnt...) cada char es un atributo.
lineToMushroom :: String -> Mushroom
lineToMushroom line = result
	where 
		mushtype =  head(line)
		attributes = filter (/=',') (tail line)
		result = (Mush mushtype attributes)


--Esta función Recibe al principio un resultado vacío que es el primer parámetro y el segundo parámetro es un Mushroom y lo que hace es, 
--a cada atributo del mushroom mira si es poisonous o edible y crea una tupla de tipo (Char,Int,Int) donde Char es el atributo y el primer Int es de poisonus y el segundo Int es de edible.
--Ejemplo (x,1,0) significa que x es poisonous y (x,0,1) significa que x es edible
--Por lo tanto la tupla mencionada se pone en una lista y se concatena con el resultado vacío que hemos pasado a la función.
generateAttributes:: [[(Char,Int,Int)]] -> Mushroom ->  [[(Char,Int,Int)]] 
generateAttributes result  (Mush tipo attributes)   = result ++ [foldl  getColumn [] attributes]
	where 
		getColumn:: [(Char,Int,Int)]  ->Char -> [(Char,Int,Int)]
		getColumn res c = if tipo == 'p' then res ++ [(c, 1, 0)] else  res ++ [(c, 0 , 1)]




--El primer parámetro es la lista de Mushrooms donde cada fila de esta lista es un Mushroom con todos sus atributos ej: [[(x,0,1)]]
--El segundo parámetro es la lista de indices (index of max) visitados.
--El tercer parámetro es la lista de los tipos de las columnas con sus nombres.(La que esta en el main: listOfNames )
--Es una función recursiva que evalua cada columna de la lista de Mushrooms y después escoje la columna con la evaluación más grande.
--Por cada tipo de esa columna que implica directamente un poisonous o edible se guardan en el outcome que es un [([(Char,Int,Int)],String)]
--donde el String es el nombre de la columna ej:(odor) y char es un tipo de la columna odor Ej:(a) y primer Int es cuantos poisonous tiene,
--el segundo Int es la cantidad de edibles que tiene. Estos tipos de columnas que implican directamente un poisonous o edible se descartan
--ya de la lista de entrada y se vuelve a hacer el mismo tratamiento mencionado recursivamente.
createDesicionTree:: [[(Char,Int,Int)]] -> [Int] -> [(String,[(Char,String)])] -> [([(Char,Int,Int)],String)] 
createDesicionTree rowsMushroom visted listOfNames
	| length rowsMushroom == 0 = []
	| otherwise = outcome 
		where
			columnseparate = separate rowsMushroom
			columnsSum = map funAdd columnseparate
			columnsSumEvaluation = map (\a -> fromIntegral (a) / fromIntegral (length rowsMushroom) ) columnsSum
			indexOfbest = getIndex 1 (head columnsSumEvaluation) 0 (tail columnsSumEvaluation) visted
			best = columnseparate  !! indexOfbest
			remaining = filter notZero best
			charsRemaining = map (\(t,p,e) -> t) remaining
			attributesWithZero = filter zero best
			newData = deleteValues rowsMushroom indexOfbest charsRemaining
			outcome = (attributesWithZero, fst(listOfNames !! indexOfbest)) : createDesicionTree newData (indexOfbest:visted) listOfNames


--El primer parámetro de esta función es la lista de que recibe tras haber aplicado la funcion mencionada arriba (generateAttributes).
--Lo que devuelve esta función es una lista de mushrooms, pero ahora por un tipo de una columuna se juntan todos sus poisonous y edibles.
--Por ejemplo el tipo b de la columna cape-shape si tiene 30 poisonous y ningún edible se representa asi :(b,30,0).
--Por lo tanto por cada columna y sus tipos se le aplica lo mismo.
--Se puede utilizar el transpose pero aprovecho que zipwith se le puede pasar una funcion asi para ahorrar trabajo a la hora de generar las lo edibles y poisonous de un tipo de colunna.
separate:: [[(Char,Int,Int)]] ->  [[(Char,Int,Int)]] 
separate att = foldl junta [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[] ] att -- cada [] tendra la evaluacio de una columna  [(tipo, p, e )] 
	where
		--Esta función junta por cada columna: su tipo y la cantidad de edibles y poisonous.
		junta:: [[(Char,Int,Int)]] -> [(Char,Int,Int)] -> [[(Char,Int,Int)]] 
		junta actualListResult row = zipWith evaux actualListResult   row




--Esta función recibe una variable donde guardar el resultado(una lista) y un tipo de una columna. Entonces por cada tipo de columna comprueba,
--si está en la lista de resultados. Si esta, hace un incremento al poisonous o al edible depende del contenido del tipo de esa columna ej: (b,p+1,e) o (b,p,e+1).
--Si no está en la lista de resultados se añade como un nuevo tipo de columna como (b,1,0) o (b,0,1).
evaux:: [(Char,Int,Int)] -> (Char,Int,Int) -> [(Char,Int,Int)]
evaux actualResult (tipo,p,e) = checkAndAdd tipo p e
	where
		--Funcion que se encarga de mirar si un tipo de columna existe en el actualResult. Si esta se hace el incremento del poisnous o el edible  de este elemnto que esta en el actualResult. 
		--Si no esta se añade el elemento a la liusta actualResult.
		checkAndAdd:: Char -> Int -> Int -> [(Char,Int,Int)]
		checkAndAdd tipoC pC eC =  if any check actualResult then addExists (actualResult) (tipoC) (pC) (eC)  else actualResult ++ [(tipoC,pC,eC)] -- si no esta lo pones en la lista de resultados directamente
			where
				--Esta función mira si dos tipos de una columna son los mismos o no.
				check :: (Char,Int,Int) -> Bool
				check (tr,_,_) = (tr == tipoC) 
				  





--Esta función se encarga de concatenar la lista de tipos de columnas que son diferentes al tipo t con los que son iguales al tipo t
--Pero esta última lista tendrá como mínimo un elemento del mismo tipo de la columna que le pasamos  (Char,Int ,Int) donde el char es el tipo, el Int es la cantidad de poisonous que tiene este tipo de columna y el segundo Int
--es la cantidad de poisonous que tiene este tipo de columna. Ejemplo (b,20,30)
addExists:: [(Char,Int,Int)] -> Char -> Int -> Int -> [(Char,Int,Int)]
addExists res t p e = filter (notTheSame) (res)  ++  increment p (filter (same) (res)) 
	where 
	--Esta función comprueba de dos tipos de una columna son diferentes.
	notTheSame :: (Char,Int,Int) -> Bool 
	notTheSame ( typeAtt,_,_ ) = typeAtt /= t
	----Esta función mira si dos tipos de una columna son los mismos.
	same :: (Char,Int,Int) -> Bool  
	same (typeAt,_,_) = typeAt == t 




--Función que incrementa si es poisonous o edible. aquí en esta lista vamos a tener como mínimo un elemento que es del mismo tipo del tipo de la columna que estamos evaluando,
--El tipo con la cantidad de poisonous y el mismo tipo con la cantidad del edible.
increment ::   Int -> [(Char,Int,Int)] -> [(Char,Int,Int)]
increment p listSameType = map addaux listSameType
	where 
		addaux:: (Char,Int,Int) -> (Char,Int,Int)
		addaux (ts,ps,es) = case p of 
			1 -> (ts, ps + 1, es)
			0 -> (ts, ps, es + 1)



--Función que se encarga de eliminar una fila de la lista de los Mushrooms el Int es el índice del tipo de la columna que no hay que eliminar.
deleteValues:: [[(Char,Int,Int)]] -> Int -> [Char] -> [[(Char,Int,Int)]]
deleteValues list i remaining = foldl rebuild [] list
	where
		rebuild:: [[(Char,Int,Int)]] -> [(Char,Int,Int)] -> [[(Char,Int,Int)]]
		rebuild result rowMushroom 
			| contains (rowMushroom !! i) = result ++ [rowMushroom] -- si la columna esta en el remaining pues cojes la fila entera.
			| otherwise = result -- si no te cargas la fila entera. 
			where 
				contains:: (Char,Int,Int) -> Bool
				contains (t,p,e)  = elem t remaining





--Función que le pasamos un tupla (que es un tipo de una columna) y comprueba si tiene un 0 en poisonous o en edible
zero :: (Char,Int,Int) -> Bool
zero (t,p,e) = if ( p == 0) || (e == 0) then True else False  

--Función que le pasamos un tupla (que es un tipo de una columna) y comprueba si el poisonous y el edible son diferentes de  zero 
notZero :: (Char,Int,Int) -> Bool
notZero (t,p,e) = if (p /= 0) && (e /= 0) then True else False

--Función que devuelve el índice del maximo elemento dentro de la lista de [Float] y cada rato va comprobando si esta dentro de la lista visited
getIndex:: Int -> Float -> Int -> [Float] -> [Int] -> Int
getIndex i max indexMax  list visited 
	| length list == 0 = indexMax
	| max < head list && not (elem i visited)   = getIndex (i+1) (head list) i (tail list) visited
	| otherwise = getIndex (i+1) (max) indexMax  (tail list) visited

--Función que recibe una lista de tipos de una columna y hace la suma de los maxs entre el edible y poisonous de cada tipo.
funAdd::[(Char,Int,Int)] -> Int
funAdd l = foldl add 0 l  
	where 
		add :: Int -> (Char,Int,Int) -> Int
		add a (tipo,p,e) = a + max p e




--Función que se encarga de generar el DecisionTree a partir de un [([(Char,Int,Int)],String)] donde el Char es un tipo de una columna el primer Int es la cantidad de poisonous,
--el segundo Int es la cantidad de edible que tiene ese tipo de una columna
--El segundo parámetro es la lista de las columnas con sus tipos (la que está en el main : ListofNames)
build:: [([(Char,Int,Int)],String)] -> [(String,[(Char,String)])] -> DecisionTree
build list listoftypes
	| length list == 1 = Node root leaves
	| otherwise = Node root (leaves ++ [build (tail list) listoftypes])
		where 
			col = head list
			root = snd col
			value = fst col
			leaves = foldl getLeaves [] value
				where
					getLeaves:: [DecisionTree]-> (Char,Int,Int) -> [DecisionTree]
					getLeaves result (t,p,e) = if p == 0 then result ++ [convert (t,"edible")] else result ++ [convert (t,"poisonous")]
						where 
							convert::(Char,String) -> DecisionTree
							convert (t,name) = Fulla (getString (root) (t) (listoftypes) ,name)




--Esta función le pasamos un nombre de Columna y tipo que es Char. Se encarga de conseguir el string entero del tipo de la columna pasado.
--Ejemplo: le pasamos como primer parámetro "cap-shape" y como segundo parámetro 'c' el resultado será "conical"
--El tercer parámetro es la lista de las columnas con sus tipos (la que está en el main: ListofNames)
getString:: String -> Char -> [(String,[(Char,String)])] -> String
getString nameRoot t list = getStringaux (filter sameName list)
	where 
	sameName :: (String,[(Char,string)]) -> Bool
	sameName (name,l) = nameRoot == name

	getStringaux:: [(String,[(Char,String)])]  -> String
	getStringaux (x:xs) = result
		where
			list = snd x
			result = snd $ head(filter sameType list) 
			sameType:: (Char,String) -> Bool
			sameType (tipo,_) = t == tipo






--Función que tiene como parámetro un DecisionTree y se encarga de imprimir por la salida el DecisionTree.
--El Int es para dejar margen
printTree:: DecisionTree -> Int -> IO()
printTree (Fulla (t,c) ) space = do 
	putStrLn(take space  (repeat ' ') ++ t ++ " => " ++ c)
printTree (Node root list ) space = do 
	putStrLn (take space  (repeat ' ') ++ root )
	printListTree list (space + 1)

--Es una función que se utiliza para la función printTree que dado una lista de DecisionTree imprime por la salida el árbol.
printListTree ::[DecisionTree] -> Int -> IO()
printListTree list space
	| length list == 1 = printTree (head list) (space + 1)
	| otherwise = do 
		printTree (head list) (space + 1)
		printListTree (tail list) (space)


--Función que permite la interacción entre el usuario y el sistema
userInteraction:: DecisionTree -> IO()
userInteraction  tree = do
	systemInteraction tree
	userInteraction tree




--Función recursiva que permite dar las predicciones al usuario
systemInteraction:: DecisionTree -> IO()
systemInteraction (Node root listTrees) = do
	putStrLn("which " ++ root ++ "?")
	userRequest <- getLine
	checkifFulla listTrees (userRequest)


--Función que recibe una lista de Decisiontrees y la entrada del usuario.
--Se encarga de comprobar si la entrada del usuario es una hoja o bien node. Si es un node hace la llamada a la función systemInteraction.
--Si es una hoja imprime el resultado por la pantalla.
checkifFulla :: [DecisionTree] -> String -> IO()
checkifFulla listTrees tipo  = printIfFulla (head listTrees)
	where
		printIfFulla :: DecisionTree -> IO()
		printIfFulla (Node root list) = systemInteraction (Node root list)
		printIfFulla (Fulla (t,c)) 
			| tipo == t = putStrLn( "result: " ++ c )
			| otherwise = checkifFulla (tail listTrees) tipo
		


		 





	










