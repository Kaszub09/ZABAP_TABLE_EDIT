# ZABAP_TABLE_EDIT
Better SM30

## FEATURES
Edit any table. Create change document automatically. Extend basic functionality if needed.
![obraz](https://github.com/Kaszub09/ZABAP_TABLE_EDIT/assets/34368953/14c40616-dbc4-48ae-88a5-4dc717ef263b)
![asd2](https://github.com/Kaszub09/ZABAP_TABLE_EDIT/assets/34368953/b2c42935-90a0-443e-ba97-f4d536121cd9)
![asd](https://github.com/Kaszub09/ZABAP_TABLE_EDIT/assets/34368953/bbf6c17d-ee3c-4ffe-923a-f4def14acc44)

## INSTALATION
Via https://github.com/abapGit/abapGit. Requires https://github.com/Kaszub09/ZABAP_SALV_REPORT. Written in ABAP 7.50.

## WHY
To streamline the process of making table available for edit and tracking changes. 

With SM30:
- Every time you change table, you need to regenerate maintenance module. No more!
- Only way to add change document to SM30 is via editing maintenance module. That and any other edits are lost after recreating maintenace module, and you must implement them again (that is if you remembered copying them before regenerating module - otherwise you need to write them again from scratch).  No more!
- Table and column on generated screen are really narrow and usually needs to be manually widened. No more!

With ZSM30
- Any changes to table are automatically reflected in transaction. Generating maintenance module is not needed.
- No code is ever lost
- Change document is implemented automatically - you can decide, whether to use it or not
- Change document can be generated even for fields not marked as such
- Transaction can be easily extended with custom logic per table (additional fields with description, make some fields non-editable, add additional validation, add new commands, edit entries before save etc. ) - instead of creating maintenance view or editing maintenance modules
- It's possible to save default layout, so every time you open table for edit columns width/layout will be ~~perfect!~~ the same

## USAGE
See examples package. Either call transaction ZABAP_TABLE_EDIT only with table name, or with name of class implementing interface ZIF_ZABAP_TABLE_EDIT.

## Known issues
- Some version of function module 'CHANGEDOCUMENT_MULTIPLE_CASE2' require change indicator field to be named 'KZ', causing shortdump otherwise - you can fix it by implementing SAP notes https://github.com/Kaszub09/ZABAP_TABLE_EDIT/issues/2#issuecomment-2065116253
