# Info about Raw Data
## weather_monthly_jungfraujoch.csv
Start: 01/01/1933, End: 01/07/2025, Frequency: Monthly <br />
ths200m0;"Air temperature 2 m above ground; homogeneous monthly mean";temperature;M;1;Float;°C <br />
th91dxmv;"Air temperature 2 m above ground; deviation of the homogeneous monthly mean of the daily maxima to the norm 9120";Temperatur;température;temperatura;temperature;M;1;Float;°C  <br />
ghs000m0;"Global radiation; homogeneous monthly mean", M;0;Integer;W/m² <br />
ths00xm0;"Ice days (maximum lower than 0 degrees C); monthly total, homogeneous";Temperatur;température;temperatura;temperature;M;0;Integer;No  <br />
shs000m0;"Sunshine duration; homogeneous monthly total";Sonne;ensoleillement;soleggiamento;sunshine;M;0;Integer;min
[Explanation of Paramter Abbreviations](https://data.geo.admin.ch/ch.meteoschweiz.ogd-nbcn/ogd-nbcn_meta_parameters.csv)

## massbalance_observation.csv

Winter/Summer/Annual mass balance (Bw/Bs/Ba) → millimeters water equivalent (mm w.e.), representing the amount of snow accumulated in winter/summer/annually converted to the equivalent depth of water.
The Equilibrium Line Altitude (ELA) -> the elevation on a glacier where annual accumulation equals annual ablation. (If temperatures rise or snowfall decreases, the ELA rises.)
Minimum and maximum elevation of glacier → Determines vertical gradient and exposure to temperature changes.

## massa_CAMELS_CH_obs_based_2161.csv
Date range: 1981-01-01 to 2020-12-01
catchment = a land area, bounded by natural high points like hills or mountains, where all water, including rainfall and snowmelt, collects and drains into a single common outlet, such as a river, lake, or the ocean

waterlevel_m: observed daily water head above sea level in meters
discharge_vol_m3s: observed catchment discharge m3/s
discharge_specific_mm: observed catchment-specific discharge (converted to millimetres per day using catchment areas)
precipitation_mm: observed daily summed precipitation in mm/d
temp_min: observed daily minimum temperature in Celsius
temp_mean: observed daily averaged temperature in Celsius
temp_max: observed daily maximum temperature in Celsius
rel_sunshine-duration: observed daily averaged relative sunshine (solar irradiance ≥ 200 W m−2) duration in %
snow_water_equiv_mm: observed daily averaged snow water equivalent in mm (=it is the height of water that would result if you melted the snow down to its liquid state)
=======
station_abbr: JUN; station_name: Jungfraujoch; station_canton:VS; station_wigos_id: 0-20000-0-06730; station_type: Climate stations; station_dataowner: MeteoSchweiz;station_data_since: 01.01.1931; station_height_masl: 3571.0; station_height_barometer_masl: 3576.0;station_coordinates_lv95_east: 2641939.0; station_coordinates_lv95_north: 1155287.0; station_coordinates_wgs84_lat: 46.547556; station_coordinates_wgs84_lon: 7.985444; station_exposition_en: pass; station_url_en: https://www.meteoswiss.admin.ch/services-and-publications/applications/measurement-values-and-measuring-networks.html#param=messnetz-klima&station=JUN <br />

## weather_monthly_sion.csv
Start: 01/01/1864, End: 01/07/2025, Frequency: Monthly <br />

station_abbr: SIO; station_name: Sion; station_canton:VS; station_wigos_id: 0-20000-0-06720; station_type: Climate stations; station_dataowner: MeteoSchweiz;station_data_since: 01.01.1864; station_height_masl: 482.0; station_height_barometer_masl: 483.0;station_coordinates_lv95_east: 2591633.0; station_coordinates_lv95_north: 1118584.0; station_coordinates_wgs84_lat: 46.21865; station_coordinates_wgs84_lon: 7.330203; station_exposition_en: plain; station_url_en: https://www.meteoswiss.admin.ch/services-and-publications/applications/measurement-values-and-measuring-networks.html#param=messnetz-klima&station=SIO <br />

## massbalance_observation.csv

Winter/Summer/Annual mass balance (Bw/Bs/Ba) → millimeters water equivalent (mm w.e.), representing the amount of snow accumulated in winter/summer/annually converted to the equivalent depth of water. <br />
The Equilibrium Line Altitude (ELA) -> the elevation on a glacier where annual accumulation equals annual ablation. (If temperatures rise or snowfall decreases, the ELA rises.) <br />
Accumulation Area Ratio -> Area of Accumulation Zone / Total Glacier Area. It is used as a climate/glacier health indicator. If the AAR is lower (e.g. 0.2), it means the glacier is losing mass and shrinking.

