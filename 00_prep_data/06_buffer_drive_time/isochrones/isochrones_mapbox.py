import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, Polygon
import numpy as np
import requests
from pathlib import Path

import time
startTime = time.time()

fn = 'unique_geocodes.csv'
out_fn = 'isochrones_mapbox.geojson'
output_path = 'polygons'
Path(output_path).mkdir(parents=True, exist_ok=True)

token = 'pk.eyJ1IjoiZmtudXRoIiwiYSI6ImNsbno1N3pmdTBjOWoya29kZTRjOGZnMzIifQ.b8gWmO4rtoHQyL58CCHyeQ'
# token = 'pk.eyJ1IjoibWFpdHJleWk5MTQ3IiwiYSI6ImNsbWxhZHN2eTA4eHYyb3Bia3MzaGZ1bHoifQ.iXruYTlCiB5v5fHQKMUx0g' # maitreyi
base_url = 'https://api.mapbox.com/isochrone/v1/mapbox/driving/'

if not Path(out_fn).exists():
    df = pd.read_csv(fn)
    df = df[df.keys()[1:]]
    df_30 = df.copy()
    df_30['drive_time'] = 30
    df_15 = df.copy()
    df_15['drive_time'] = 15
    df_60 = df.copy()
    df_60['drive_time'] = 60
    df = pd.concat([df_30, df_15, df_60])
    df['geometry'] = None
    df = df.reset_index()
    gdf = gpd.GeoDataFrame(df, geometry=df['geometry'].values)
    gdf.crs= 'epsg:4326'
else:
    gdf = gpd.read_file('isochrones_mapbox.geojson')

c = 0
for i in range(len(gdf)):
    
    gdf_tmp = gdf[gdf.index == i].copy()
    lon = gdf.iloc[i].LONG
    lat = gdf.iloc[i].LAT
    contours_minutes = gdf.iloc[i].drive_time
    print(i, lon, lat, contours_minutes)

    out = [str(x) for x in ['isochrone_mapbox', i, str(lon).replace('-','W'), 'N'+str(lat), contours_minutes, ]]

    out_fn = '_'.join(out).replace('.','_') + '.geojson'
    out_fn_parts = out_fn.split('_')
    out_fn_parts[2] = out_fn_parts[2].rjust(6,'0')
    out_fn_parts[4] = out_fn_parts[4].ljust(5, '0')
    out_fn_parts[6] = out_fn_parts[6].ljust(5, '0')
    out_fn = '_'.join(out_fn_parts)
    
    out_fp = Path(output_path, out_fn)
    
    
    if not out_fp.exists():
        try:
            url = base_url + str(lon)+','+str(lat)+'?'+ 'contours_minutes='+str(contours_minutes)+\
              '&polygons=true&access_token='+token
    
            r = requests.get(url)
            print(r)
            if r.ok:
                try:
                    p = Polygon(r.json()['features'][0]['geometry']['coordinates'][0])
                    if isinstance(p, Polygon):
                        gdf_tmp['geometry'] = p
                        gdf_tmp.to_file(out_fp)
                        c+=1
                        print('Polygon created')
                        print(str(out_fp))
                except:
                    print('WARNING: no polygon created for', i, lon, lat, contours_minutes)
                    print(r.json())
            # if c == 1:
            #     break
        except:
            continue
            
    
executionTime = (time.time() - startTime)
print('Execution time in seconds: ' + str(executionTime))
print('DONE')