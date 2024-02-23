# ZABAP_TABLE_EDIT
Better SM30

## FEATURES
Edit any table. Create change document automatically. Extend basic funcitonality if needed.
![obraz](https://github.com/Kaszub09/ZABAP_TABLE_EDIT/assets/34368953/14c40616-dbc4-48ae-88a5-4dc717ef263b)
![obraz](https://github.com/Kaszub09/ZABAP_TABLE_EDIT/assets/34368953/e8750c24-5f8e-40b6-b3b3-c5c827e5ed01)

## INSTALATION
Via https://github.com/abapGit/abapGit. Requires https://github.com/Kaszub09/ZABAP_SALV_REPORT.

## WHY
To streamline the process of making table available for edit and tracking changes. 

With SM30:
- Every time you change table, you need to regenerate maintenance view. No more!
- Only way to add change document to SM30 is via editing maintenance view. That and any other edits are lost after recreating maintenace view, and you must implement them again (that is if you remembered copying them before regenerating view - otherwise you need to write them again from scratch).  No more!
- Table and column on generated screen are really narrow and usually needs to be manually widened. No more!

With ZSM30
- Any changes are autmatically reflected in transaction
- No code is ever lost
- Change document is implemented automatically for all tables - you can decide, whether to use it or not
- Change document can be generated even for fields not marked as such
- Transaction can be easily extended with custom logic per table (additional fields with description, make some fields non-editable, add additional validation, add new commands, edit entries before save etc. ) - instead of creating maintenance view
- It's possible to save default layout, so every time you open table for edit the columns width will be perfect!

## USAGE
See examples package. Either call transaction only with table name, or with name of class implementing interface ZIF_ZABAP_TABLE_EDIT.
