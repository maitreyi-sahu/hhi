Raw 2000-2019 AHA data lives here: */mnt/share/resource_tracking/us_value/data/hospital_hhi/raw/aha/AHA raw data*

Formatted 2000-2019 AHA data lives here:
*/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/*

These files load and format the AHA data:
1. clean, map to HRR, create state-HRRs
2. manually clean the lat/lon geocodes
3. using county boundaries, assign hospitals to counties [this takes long and some is done locally!]
4. using mcnty shapefile, assign hospitals to mcnty
5. create list of hospital IDs which can be sourced as needed

Gen/Surg hospitals include:
+ SERV == 10: General medical and surgical
+ SERV == 13: Surgical

**NOTE - 'cnty' and 'cnty_name' variables still need to be cleaned!!**