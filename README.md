# leviathan_rest_lib
`leviathan_rest_lib` provides a RESTish interface to the leviathan library 
functions.

# Example upload CEN file
Uploads and compiles wiring for CENs described in a CEN file.

```
curl -d @/tmp/cen.json http://<location>:8080/cen
```
(HTTP POST of the CEN file).

# Example CEN file
```
{"cenList":
 [{
     "cenID" : "cen1",
     "containerIDs" : [ "c1","c2","c13","c14"]
  },
  {
      "cenID":"cen2",
      "containerIDs":["c4","c5","c6","c7"]
  },
  {
      "cenID":"cen3",
      "containerIDs":["c15","c16","c9","c10","c11"]
  },
  {
      "cenID":"cen4",
      "containerIDs":["c11","c12"]
  },
  {
      "cenID":"cen5",
      "containerIDs":["c2","c3"]
  }]
}
```

# Example prepare CENs
Prepares a list of CENS on a host

This creates all artifacts like network namespaces, bridges, interfaces, etc 
```
curl -d '["cen1","cen2","cen3","cen4","cen5"]' http://<location>:8080/cen/prepare
```
(HTTP POST of a JSON list of the CENs to prepare).


# Example destroy CENs
Destroys a list of CENS on a host
This destroy all artifacts like network namespaces, bridges, interfaces, etc 
```
curl -d '["cen1","cen2","cen3","cen4","cen5"]' http://<location>:8080/cen/destroy
```
(HTTP POST of a JSON list of the CENs to prepare).

