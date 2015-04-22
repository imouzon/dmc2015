#Feature Description

- Column Name: The name of the variable as it appears in the feature matrix
- NAs?: Do the values exist for every entry
- Type: Left blank if no category applies. H = historical data used (uses data about previous orders), C = complete data used (aggregates information about all orders), C0 = complete data used except for current observation
- Ys?: Does the feature depend on our outcomes (the `y`s). Yes if the feature depends on known `couponUsed` and `basketValue`, No if it is not based on the responses we are modeling
- inX: is the feature in the current version of the feature matrix yet? (this is No if you are just now adding this feature)

| Column Name | NAs? | Type |  Ys? | Description |
|:------------|:----:|:----:|:----:|:------------|
