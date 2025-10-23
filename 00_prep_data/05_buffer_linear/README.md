There are 3 versions of the "linear buffer" method"

1. Linear buffer with UTM zone - our corrected method
2. Linear buffer with CRS 3857 - will have a slightly different radius size for different coordinates based on east/west
3. Linear buffer using "haversine" - only Harry used this, for 2019-2022, which is a shortcut using angular distance

The "correct" one is the "linear buffer with UTM zone", but need to come back to **03_check_hhi.R** and do some comparison for appendix figures.

Maitreyi Sahu
4/7/2024

