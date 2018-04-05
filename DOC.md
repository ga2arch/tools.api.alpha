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
Routes go under the keyword `:routes` and can be of two types:

### Declaration
*   ```clojure
    [:context :v1 "/base"]
    <details>
    ```
    | key | description |
    | ----- | ----------- |
    | `:v1` | the version of the context |
    | "/base" | the base path of the group |
    
    which defines a context under which a set of routes can be grouped, all the keys will be merged 
    with all the keys defined in each sub route except for `:include` and `:routes`, every path in the subroute 
    will be prepended with the version + basepath.
    
*   ```clojure
    [:POST "/requests/{request_uid}"]
    <details>
    ```
    | key | description |
    | ----- | ----------- |
    | `POST` | the http method |
    | "/requests/{request_uid}" | the base path of the group |
    | "{request_uid}" | the path param |

### Details
The details of a route have the following schema:

| key               | description                        | type                             |
| ----------------- | ---------------------------------- | -------------------------------- |
| `:description`    | the description of the route         | `string` |
| `:include`        | refer to include section           | |
| `:path-params`          | refer to path-params section             | |
| `:query-params`          | refer to query-params section            | |
| `:body`          | refer to the body section            | |
| `:return`          | refer to the return section section            | |

#### Path Params 
A map where the key is a symbol that has to match the one in the path and the value
can be a keyword (the type) or a vector with `[<type> <description> <opts>]`

example:
```clojure
 {request_uid :type/uuid}
```

```clojure
{request_uid [:type/uuid "the uid of the request"]}
```

path params maps is merged with the path params declared in a context of the route (if any)

#### Query Params
coming soon...

#### Body
coming soon...

#### Return
coming soon...

