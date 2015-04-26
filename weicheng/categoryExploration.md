# Category analysis


## Q0: How many unique categories?

### Ans: 31

## Q1: We know some coupons belong to several categories, but what is the maximum number of categories they have?

### Ans: 5


```
## [1] 5
```
## Q2: Is coupon usage rate and the basketValue related to the number of categories it belongs to?

### Ans: Not quite significant but yes. For example, for coupons in `couponID2`, if their `categoryIDs2` has five categories, then their coupon usage rate seems to be much lower.

Stat for `categoryIDs1`. Row i stands for `categoryIDs1` that has i categories.


```
##         totalNum coup1UsedNum coup1UsedRatio meanBasketValue
## catNum1     3014          696      0.2309224        333.8813
## catNum2     2438          603      0.2473339        302.6293
## catNum3      556          129      0.2320144        279.1817
## catNum4       40            8      0.2000000        217.1043
## catNum5        5            2      0.4000000        285.2320
##         medianBasketValue
## catNum1           211.985
## catNum2           213.235
## catNum3           210.780
## catNum4           197.025
## catNum5           289.250
```

Stat for `categoryIDs2`


```
##         totalNum coup2UsedNum coup2UsedRatio meanBasketValue
## catNum1     3172          624      0.1967213        303.7397
## catNum2     1869          352      0.1883360        353.9686
## catNum3      708          100      0.1412429        262.5883
## catNum4      203           41      0.2019704        287.7426
## catNum5      101           12      0.1188119        397.1282
##         medianBasketValue
## catNum1            213.28
## catNum2            211.03
## catNum3            211.80
## catNum4            211.82
## catNum5            224.23
```

Stat for `categoryIDs3`


```
##         totalNum coup3UsedNum coup3UsedRatio meanBasketValue
## catNum1     2573          508      0.1974349        295.9553
## catNum2     2167          320      0.1476696        342.6381
## catNum3      983          121      0.1230926        276.8410
## catNum4      313           54      0.1725240        408.6745
## catNum5       17            5      0.2941176        319.0924
##         medianBasketValue
## catNum1            211.78
## catNum2            212.47
## catNum3            213.40
## catNum4            217.19
## catNum5            249.24
```

## Q3: Is the coupon usage rate and the basketValue related to categories?

### Ans: Yes. For exmaple, for coupoons in `couponID2`, if they belong to `category8`, then the coupon usage rate is as high as 0.42!

Stat for `categoryIDs1`


```
##            totalNum coup1UsedNum coup1UsedRatio meanBasketValue
## category1      3750          760     0.20266667        286.0484
## category2       475          103     0.21684211        275.3145
## category3       444          101     0.22747748        387.4121
## category4       993          274     0.27593152        267.9785
## category5      1196          390     0.32608696        385.6312
## category6       393          134     0.34096692        271.2327
## category7       260           64     0.24615385        277.4145
## category8       206           74     0.35922330        331.8960
## category9      1237          249     0.20129345        300.9269
## category10      268           65     0.24253731        317.5348
## category11       32            3     0.09375000        298.3088
## category12      108           26     0.24074074        268.2787
## category13       51           15     0.29411765        458.9496
## category14       26            7     0.26923077        257.1950
## category15       56           13     0.23214286        234.1489
## category16       65            7     0.10769231        314.8283
## category17       25            9     0.36000000        281.6752
## category18        8            2     0.25000000        243.7700
## category19       29            9     0.31034483        235.6121
## category20       62           12     0.19354839        775.8384
## category21       13            1     0.07692308        320.5831
## category22        3            1     0.33333333        276.2833
## category23       13            1     0.07692308        206.6100
## category24       10            2     0.20000000        217.9440
## category25        2            2     1.00000000        294.5750
## category26        2            2     1.00000000        294.5750
## category27        2            2     1.00000000        294.5750
## category28       14            3     0.21428571        322.3286
## category29        0            0            NaN             NaN
## category30        0            0            NaN             NaN
## category31        0            0            NaN             NaN
##            medianBasketValue
## category1            214.900
## category2            212.660
## category3            205.290
## category4            217.930
## category5            210.805
## category6            203.720
## category7            197.510
## category8            231.245
## category9            212.930
## category10           210.200
## category11           258.850
## category12           210.710
## category13           194.410
## category14           210.895
## category15           194.410
## category16           210.150
## category17           212.790
## category18           207.195
## category19           203.020
## category20           218.725
## category21           282.950
## category22           290.920
## category23           216.360
## category24           192.695
## category25           294.575
## category26           294.575
## category27           294.575
## category28           284.045
## category29                NA
## category30                NA
## category31                NA
```

Stat for `categoryIDs2`


```
##            totalNum coup3UsedNum coup3UsedRatio meanBasketValue
## category1        33            4      0.1212121        905.1824
## category2      1007          159      0.1578947        278.6589
## category3      1719          332      0.1931355        308.7463
## category4      1169          203      0.1736527        273.2202
## category5        81           30      0.3703704        284.3114
## category6       333           72      0.2162162        684.2573
## category7       118           26      0.2203390        313.6011
## category8       204           86      0.4215686        301.4555
## category9       717          114      0.1589958        312.0656
## category10      251           33      0.1314741        292.5612
## category11      315           44      0.1396825        305.0478
## category12       45            7      0.1555556        348.0293
## category13       52           11      0.2115385        251.4192
## category14       33            7      0.2121212        274.5645
## category15      101           21      0.2079208        299.0324
## category16     3339          504      0.1509434        317.1251
## category17       65           17      0.2615385        262.5520
## category18        6            1      0.1666667        213.0033
## category19      372          106      0.2849462        296.1585
## category20      172           38      0.2209302        288.3330
## category21      144           21      0.1458333        351.3466
## category22        3            0      0.0000000        278.3233
## category23        0            0            NaN             NaN
## category24       55           11      0.2000000        240.2544
## category25        1            0      0.0000000        384.0000
## category26        1            0      0.0000000        384.0000
## category27        1            0      0.0000000        384.0000
## category28        0            0            NaN             NaN
## category29       10            3      0.3000000        374.2500
## category30        1            0      0.0000000         94.1500
## category31        3            2      0.6666667        213.5333
##            medianBasketValue
## category1            199.970
## category2            210.900
## category3            209.270
## category4            213.160
## category5            211.030
## category6            212.790
## category7            204.895
## category8            219.555
## category9            212.050
## category10           211.450
## category11           218.680
## category12           221.870
## category13           196.080
## category14           203.760
## category15           224.790
## category16           214.040
## category17           208.530
## category18           203.950
## category19           207.560
## category20           206.960
## category21           230.345
## category22           305.370
## category23                NA
## category24           211.080
## category25           384.000
## category26           384.000
## category27           384.000
## category28                NA
## category29           244.865
## category30            94.150
## category31           235.670
```

Stat for `categoryIDs3`


```
##            totalNum coup3UsedNum coup3UsedRatio meanBasketValue
## category1        56           12      0.2142857        252.2525
## category2      1577          204      0.1293595        320.1235
## category3      4901          733      0.1495613        311.0833
## category4      1289          180      0.1396431        273.1058
## category5        99           35      0.3535354        269.7178
## category6       231           57      0.2467532        264.5541
## category7        40            8      0.2000000        430.0523
## category8       236           72      0.3050847        327.2028
## category9       828          113      0.1364734        267.3408
## category10      611           87      0.1423895        279.6237
## category11      136           20      0.1470588        583.7332
## category12       79           16      0.2025316       1996.9242
## category13       20            6      0.3000000        280.6290
## category14       78           24      0.3076923        501.6349
## category15      344           60      0.1744186        283.3616
## category16       43            6      0.1395349        672.4895
## category17      140           34      0.2428571        337.5324
## category18       25            5      0.2000000        338.2664
## category19       27            9      0.3333333        373.0981
## category20      293           43      0.1467577        418.4777
## category21       70           12      0.1714286        245.1370
## category22        8            1      0.1250000        249.1713
## category23        2            0      0.0000000        693.4700
## category24       28            4      0.1428571        228.6739
## category25        6            3      0.5000000        273.8550
## category26        6            3      0.5000000        273.8550
## category27        6            3      0.5000000        273.8550
## category28        3            0      0.0000000        274.9133
## category29        0            0            NaN             NaN
## category30        0            0            NaN             NaN
## category31       11            2      0.1818182        203.8109
##            medianBasketValue
## category1            215.040
## category2            218.860
## category3            212.190
## category4            213.580
## category5            211.030
## category6            204.370
## category7            206.315
## category8            210.455
## category9            209.625
## category10           216.180
## category11           215.340
## category12           206.130
## category13           200.590
## category14           232.360
## category15           204.690
## category16           221.270
## category17           244.285
## category18           215.340
## category19           185.290
## category20           218.210
## category21           209.510
## category22           165.210
## category23           693.470
## category24           197.330
## category25           258.525
## category26           258.525
## category27           258.525
## category28           241.140
## category29                NA
## category30                NA
## category31           196.960
```

