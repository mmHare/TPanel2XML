# Changelog

## [2025-12-20]

### Added

- Second option for binding components with XML node names.
- Two different class constructors:
	`CreateWithTags(APanel: TPanel; ADictNodeNames: TDictionary<Integer, string>);` - TComponentXmlBuilder will use previously introduced component Tag mapping
	`Create(APanel: TPanel);` - will depend on component's Name property

### Changed

- *AddCustomComponentValue* now accepts value as Variant instead of String only

## [2025-12-15]

### Added

- Custom save and custom load options

### Changed

- Project and repository rename

## [2025-12-14]

### Added

- Load XML function.
- Support for TGroupBox, TRadioGroup and TMemo.
- Encryption option for TEdits with set PasswordChar property.

## [2025-12-13]

### Added

- Initial release.
- Support for components: TPanel, TEdit, TSpinEdit, TComboBox, TCheckBox.
- Node save order according to components TabOrder.
- Demo project.