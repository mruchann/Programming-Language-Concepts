module PE1 where

import Text.Printf

-- Recipe = Recipe Name [(Ingredient, Quantity)]
data Recipe = Recipe String [(String, Double)] deriving Show

-- Price = Price Ingredient Quantity Price
data Price = Price String Double Double deriving Show

getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x


getIngredientCost :: (String, Double) -> [Price] -> Double
getIngredientCost (wantedIngredient, wantedQuantity) prices = getRounded (findCost prices)
    where 
        findCost (Price ingredient quantity price: rest)
            | ingredient == wantedIngredient = wantedQuantity/quantity * price
            | otherwise = findCost rest

recipeCost :: Recipe -> [Price] -> Double
recipeCost (Recipe name ingredients) prices = 
    getRounded (
        sum [getIngredientCost (wantedIngredient, wantedQuantity) prices | (wantedIngredient, wantedQuantity) <- ingredients]
    )

getQuantity :: String -> [(String, Double)] -> Double
getQuantity wantedIngredient list =  getRounded (sum [
        quantity | (ingredient, quantity) <- list, ingredient == wantedIngredient
    ])

missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]
missingIngredients (Recipe name ingredients) stockList = [
    (wantedIngredient, getRounded (wantedQuantity - getQuantity wantedIngredient stockList) ) 
        | (wantedIngredient, wantedQuantity) <- ingredients, wantedQuantity > getQuantity wantedIngredient stockList 
    ]

makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe stockList (Recipe name ingredients) = if missingIngredients (Recipe name ingredients) stockList == []
    then [
    (ingredient, getRounded (stockQuantity - getQuantity ingredient ingredients) )
        | (ingredient, stockQuantity) <- stockList, stockQuantity > getQuantity ingredient ingredients
    ] 
    else stockList

mergeAllRecipeIngredients :: [Recipe] -> [(String, Double)]
mergeAllRecipeIngredients recipes = concatMap (\(Recipe name ingredients) -> ingredients) recipes

calculateQuantities :: [(String, Double)] -> [(String, Double)]
calculateQuantities mergedIngredients = [(ingredient, getRounded (getQuantity ingredient mergedIngredients)) | (ingredient, quantity) <- mergedIngredients] 

removeDuplicatesIngredients :: [(String, Double)] -> [(String, Double)]
removeDuplicatesIngredients mergedCalculatedIngredients = removeDuplicates mergedCalculatedIngredients 
    where
        removeDuplicates [] = []
        removeDuplicates (x:xs)
            | x `elem` xs = removeDuplicates xs
            | otherwise = x : removeDuplicates xs

makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double, Double)]
makeShoppingList stockList recipes prices = [(ingredient, getRounded quantity, getRounded (getIngredientCost (ingredient, quantity) prices))
    | (ingredient, quantity) <- missingIngredients (Recipe "shoppingList" (removeDuplicatesIngredients (calculateQuantities (mergeAllRecipeIngredients recipes)))) stockList]