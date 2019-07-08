ifEven myFunction x = if even x
    then myFunction x
    else x

inc x = x + 1

genIfEven f = \x -> ifEven f x

-- 5.2

getRequestURL host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id -> getRequestURL host apiKey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

