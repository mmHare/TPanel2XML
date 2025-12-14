# TPanel2XML

A helper stand-alone (one file [uPanel2XML.pas](src/CHANGELOG.md)) class that parses VCL Components from TPanel as XML nodes structure. Can be used for faster development of forms that are linked to XML data, e.g. configuration window. Just put settings on panel and pass it to be saved as XML file.
For example usage see Demo project.

## Working principle

TPanel2XML class accepts TPanel that will be treated as root element and owned components will become child nodes. Second required parameter is *<Integer, String>* Dictionary which will link node names to components.
Tag property value of a component will be looked up in dictionary keys and corresponding value will be node name; node text is taken from component respective value.

1. Create root panel, put components or group them with other panels.
2. Define node name dictionary.
3. Assign components (also panels, including the root one).
4. (Optional) Set Tab order for components.
5. Create TPanel2XML, pass root panel component and node name dictionary. Change additional options if needed.
6. Save to XML file.

## Supported components

| Component | Representing element                                                      |
|-----------|---------------------------------------------------------------------------|
| TPanel    | Element node containing child nodes                                       |
| TEdit     | Element with text value                                                   |
| TSpinEdit | Element with numeric text value                                           |
| TComboBox | Element with value from ItemIndex                                         |
| TCheckBox | Element with boolean (checked) value - can be in 1/0 or true/false format |

## Additional properties

- *BoolStrValue* - if *True*, boolean values will be saved as 'true/false'; if False - '1/0' (default *False*)
- *WithTabOrder* - elements (within common panel parent) will be sorted with components TabOrder property and saved in ascending order; if False the order will be determined by application creation order (default *True*)

## Side notes

If component has Tag value that is not in names dictionary it will be ignored. This can be used to exclude certain values from being saved but also it is important to set correct Tag numbers.