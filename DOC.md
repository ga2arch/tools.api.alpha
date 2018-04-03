# Documentation

The api schema is written in edn with the following format 

| key               | description                        | type                             |
| ----------------- | ---------------------------------- | -------------------------------- |
| `:schema-version` | version of the schema              | `string` |  
| `:base-path`      | maps with for key the env and for value the base path of the service | `map` |  
| `:description`    | the description of service         | `string` |
| `:include`        | refer to include section           | |
| `:types`          | refer to types section             | |
| `:routes`          | refer to routes section            | |

## Include
the `:include` key allows the following formats:
* `:include "file.edn"`  
the map in file.edn will be deep merged with the map in which include is defined, the order of the merge
is left-to-right where left is the original map and right is the map to merge. That means the keys in the merged map will overwrite
the keys in the original map if they are simple values, if they are map they will be merged again.

* `:include ("file.ed" GenericType)`  
referred as "parametrized include" the included map has to have a key called 
`:signature []` with a map of generics types that will be bind-ed with the args passed and substituted in all occurrences of the 
generic type in the map. 
example:
    ```clojure
    {:signature    [T]
     :query-params {starting_after [:type/string "the starting after" :required]
                    limit          :type/integer}
     :consumes     [:application/json]
     :produces     [:application/json]
     :return       {200 {(paginated-list T)
                         {:has_more :type/boolean
                          :data     [T]}}}}
    
    ```
    
    will become before being merged:
    
    ```clojure
    {:query-params {starting_after [:type/string "the starting after" :required]
                    limit          :type/integer}
     :consumes     [:application/json]
     :produces     [:application/json]
     :return       {200 {(paginated-list GenericType)
                         {:has_more :type/boolean
                          :data     [GenericType]}}}}
    
    ```

* `:include ["file.edn" ("file1.ed" GenericType)`]  
multiple files can be included by passing them into a vector

## Types
types are of two kind:

* concrete
    ```clojure
    ApiRequestBean
    {id          :type/string
    session_id  :type/uuid
    insert_date :type/date}
    ```

* generic
    ```clojure
    (paginated-list T)
    {has_more :type/boolean
     data     [T]}
    ```
    
the simple types are:

* `:type/string` 
* `:type/integer`
* `:type/int`
* `:type/void`
* `:type/uuid`
* `:type/date`
* `:type/boolean`

types can be defined inline or under the key `:types` so that they can be reused.

## Routes
coming soon ...

