# TComponentXmlBuilder

A small Delphi utility that serializes VCL component hierarchy to and from XML.
It is designed for configuration forms, allowing UI controls placed on a container (such as TPanel) to be automatically saved to and loaded from XML using a simple component-to-node mapping.

## Working principle

TComponentXmlBuilder class accepts TPanel that will be treated as root element and owned components will become child nodes. Second required parameter is *<Component.Tag Integer, ELEMENT_NAME string>* Dictionary which will link node names to components.
Tag property value of a component will be looked up in dictionary keys and corresponding value will be node name; node text is taken from component respective value.

1. Create root panel, put components or group them with other panels.
2. Define node name dictionary.
3. Assign components (also panels, including the root one).
4. (Optional) Set Tab order for components.
5. Create TComponentXmlBuilder object, pass root panel component and node name dictionary. Change additional options if needed.
6. Save/load XML file.

## Supported components

| Component   | Representing element                                            | Component property |
|-------------|-----------------------------------------------------------------|--------------------|
| TPanel      | Element containing child nodes                                  | --- |
| TGroupBox   | Element like TPanel but cannot be used as main panel            | --- |
| TEdit       | Element with text value (option for encryption)                 | Text, encryption if PasswordChar |
| TMemo       | Element with text value                                         | Text(save)/Lines(load) |
| TSpinEdit   | Element with numeric text value                                 | Value |
| TComboBox   | Element with numeric value of ItemIndex                         | ItemIndex |
| TRadioGroup | Element with numeric value of ItemIndex                         | ItemIndex |
| TCheckBox   | Element with boolean (checked) value (1/0 or true/false format) | Checked |

## Additional properties

- *BoolStrValue* - if *True*, boolean values will be saved as 'true/false'; if False - '1/0' (default *False*)
- *WithTabOrder* - elements (within common panel parent) will be sorted with components TabOrder property and saved in ascending order; if False the order will be determined by application creation order (default *True*)
- *OnEncodeText* and *OnDecodeText* (function(string):string) - if methods are assigned then TEdit components with not empty *PasswordChar* will be encoded/decoded using them

## Side notes

If component has Tag value that is not in names dictionary it will be ignored. This can be used to exclude certain values from being saved but also it is important to set correct Tag numbers.
Passwords in demo project are obfuscated, not encrypted. This is for demonstration purposes only.

Code was written in Delphi 12 and might not be compatible with previous versions. I plan to check it against Delphi 10.3 in the future.