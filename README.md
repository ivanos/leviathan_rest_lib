# leviathan_rest_lib
`leviathan_rest_lib` provides a RESTish interface to the leviathan library 
functions.

# API
URI | Method | Body | Description
--- | ------ | ---- | -----------
/cin | POST | JSON file | upload JSON file
/cin/prepare | POST | list of Cin Ids | prepare CINs
/cin/destroy | POST | list of Cin Ids | undo CINs
/cen | POST | JSON file | upload JSON file
/cen/prepare | POST | list of Cen Ids | prepare CENs
/cen/destroy | POST | list of Cen Ids | undo CENs
/host/HostId/ContainerId/CenId | PUT | none | add Container to Cen
/host/HostId/ContainerId/CenId | DELETE | none | remove Container from Cen
/cen/CenId | PUT | none | create CEN
/cen/CenId | GET | none | get CEN structure
/cen/CenId | DELETE | none | remove CEN
/host/HostId/ContainerId | GET | none | get Container structure
/host/HostId/ContainerId | PUT | none | add Container to host
/host/HostId/ContainerId | DELETE | none | remove Container from host

## CEN Structure
```
{
    "cenID": CenId,
    "contIDs": [ContId1, ContId2, ...],
    "wiring_type": WiringType
}
```
Where `contIDs` is the list of containers in the CEN.  `WiringType` is
`"bridge"`, `"wire"`, or `null`.

## Example CEN file
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

# Examples

## Prepare CENs
Prepares a list of CENS on a host

This creates all artifacts like network namespaces, bridges, interfaces, etc 
```
curl -d '["cen1","cen2","cen3","cen4","cen5"]' http://<location>:8080/cen/prepare
```
(HTTP POST of a JSON list of the CENs to prepare).


## Destroy CENs
Destroys a list of CENS on a host

This destroy all artifacts like network namespaces, bridges, interfaces, etc 
```
curl -d '["cen1","cen2","cen3","cen4","cen5"]' http://<location>:8080/cen/destroy
```
(HTTP POST of a JSON list of the CENs to destroy).


## Add Container to Cen
```
curl -v -X PUT -H "content-type: application/json" http://localhost:8080/host/host1/container1/cen1
```

## Remove Container from Cen
```
curl -v -X DELETE -H "content-type: application/json" http://localhost:8080/host/host1/container1/cen1
```

## Upload CEN file
Uploads and compiles wiring for CENs described in a CEN file.

```
curl -d @/tmp/cen.json http://<location>:8080/cen
```
(HTTP POST of the CEN file).
