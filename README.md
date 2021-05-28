# lambda_expression
(\x.x x) -> x
(\x.x) -> Error
((\x.\y.(x y) x) y) -> (x y ) # для двух аргументов
(\x.x \y.y) x y -> λv2.v2 # \y.y будет вставлен \x.x, а x y игнорируется
