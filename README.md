# TComponentXmlBuilder

A small Delphi utility that serializes VCL component hierarchy to and from XML.
It is designed for configuration forms, allowing UI controls placed on a container (such as TPanel) to be automatically saved to and loaded from XML using a simple component-to-node mapping.

## Working principle

TComponentXmlBuilder class accepts TPanel that will be treated as root element and owned components will become child nodes. The builder will loop through all contained controls saving those bound to XML elements. Node text is taken from component respective value (see the table below). There are two ways for binding components to node names (each is defined by different class constructor):

A) Binding XML element with component __Name__ property. Uses Create constructor expecting only root panel. Components shall be added by *AddComponentBind* and provided with node name. Loop will look for component names in the structure.

1. Create root panel, put components or group them with other panels.
2. Create TComponentXmlBuilder object and pass root panel component.
3. Add components with *AddComponentBind* (also panels, including the root one).
4. (Optional) Set Tab order for components. 
5. Change additional options if needed.
6. Save/load XML file.

B) Binding XML element component __Tag__ property. Constructor *CreateWithTags* requires *<Component.Tag Integer, ELEMENT_NAME string>* Dictionary that will be used for mapping, link node names to components. Tag property value of a component will be looked up in dictionary keys and corresponding value will be node name.

1. Create root panel, put components or group them with other panels.
2. Define node name dictionary.
3. Assign components (also panels, including the root one).
4. (Optional) Set Tab order for components.
5. Create TComponentXmlBuilder object, pass root panel component and mapping dictionary. Change additional options if needed.
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
- Custom save option - with *AddCustomComponentValue* components can be bound to explicitly defined value. Bound XML element will be saved with provided value instead of value from component itself. This can be used for overwriting values or bind not supported controls (e.g. TMaskEdit).
- Custom read option - analogously, *AddToReadList* and *GetComponentValue* can be used for explicit read of bound XML elements. Add components to read list before loading XML and their respective XML values will be stored in a list instead of updating component value. Use *GetComponentValue* to read it and process it in a way you need it. This is also a way to handle not supported controls. Returned value is a string.

## Side notes

- If component has Tag value that is not in names dictionary it will be ignored. This can be used to exclude certain values from being saved but also it is important to set correct Tag numbers.
- TPanel that is not bound to node name will be ignored (including owned components).
- Custom save option can be used with some dummy components for static text - like TLabel which doesn't have edit value (however TLabel doesn't have TabOrder property so it might break the ordering, so it is recommended to use components that have this property). Similarly custom read option can be used with dummy to read specific XML element, as well as exclude components from updating their values.
- Passwords in demo project are obfuscated, not encrypted. This is for demonstration purposes only.

- Code was written in Delphi 12 and might not be compatible with previous versions. I plan to check it against Delphi 10.3 in the future.