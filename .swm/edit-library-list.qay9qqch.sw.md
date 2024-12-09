---
title: Edit Library List
---
# Introduction

This document will walk you through the "Edit Library List" code change.

The purpose of this change is to ensure that the necessary libraries are added to the library list for the program to function correctly.

We will cover:

1. Why we need to add multiple libraries to the library list.
2. How we handle potential errors when adding libraries.
3. The overall structure of the program.

# Adding multiple libraries

<SwmSnippet path="/SOURCE/tdlib.pgm.clle" line="1">

---

We need to add multiple libraries to the library list to ensure that all required resources are available for the program. Each library contains specific resources that the program depends on.

```
pgm
  addlible TDDEV    *last
  monmsg cpf9999
  addlible TDLIB    *last
  monmsg cpf9999
  addlible DBU11    *last
  monmsg cpf9999
  addlible SQLTOOLS *last
  monmsg cpf9999
  addlible OLFILE5  *last
  monmsg cpf9999
  addlible CONPGM   *last
  monmsg cpf9999
  addlible OLPGM5D  *last
  monmsg cpf9999
  addlible WMSPGM2  *last
  monmsg cpf9999
  addlible YDPGM3   *last
  monmsg cpf9999
endpgm
```

---

</SwmSnippet>

# Handling potential errors

We use the <SwmToken path="/SOURCE/tdlib.pgm.clle" pos="3:1:3" line-data="  monmsg cpf9999">`monmsg cpf9999`</SwmToken> command after each <SwmToken path="/SOURCE/tdlib.pgm.clle" pos="2:1:1" line-data="  addlible TDDEV    *last">`addlible`</SwmToken> command to handle any potential errors that might occur when adding a library. This ensures that the program continues to run even if a library cannot be added.

# Overall structure

The program starts with the <SwmToken path="/SOURCE/tdlib.pgm.clle" pos="1:0:0" line-data="pgm">`pgm`</SwmToken> command and ends with the <SwmToken path="/SOURCE/tdlib.pgm.clle" pos="20:0:0" line-data="endpgm">`endpgm`</SwmToken> command. This defines the boundaries of the program and ensures that all commands within these boundaries are executed in sequence.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBdGRsaWIlM0ElM0F0aW0tZHVuZ2Fu" repo-name="tdlib"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
