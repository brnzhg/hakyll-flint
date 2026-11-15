---
title: Recipes
---
Start with [[/recipes/lentil-soup.md#method]].

## Thai chicken

$for(pagesWhere("ingredient", "chicken", "cuisine", "thai"))$
- [$title$]($url$)
$endfor$

## All ingredients

$for(allIngredients)$
- [$value$]($url$) ($count$)
$endfor$
