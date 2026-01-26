# iCLEANED Development Work 2025
This fork documents development work conducted during the year 2025. The development specifications include:

## Feature/tables ux improvements
Improvement of parameters database module and data management system aiming to enhance user experience, data integrity, and code quality.

## Issues Addressed
✅ Removing sub-climate selectInput on the Farm section. 
✅ Improving user experience on the Crop inputs table. 
✅ Improving parameters database tables navigation. 
✅ Improving Feedtype table experience in the parameters database. 
✅ Improving Feeditem table experience in the parameters database.
✅ Changing variable names into more suited ones across the app.
✅ Removing redudant variables from the code.

## Datasets Major Changes
1. lkp_feedtype
- Rename 'lkp_feedtype' to 'lkp_crops'.
- Renamed 'lkp_feedtype' to 'lkp_crops' in parameter database references.
- Updated all 'lkp_feedtype()' calls to 'lkp_crops()' in server modules.
- Renamed CSV files from 'lkp_feedtype.csv' to 'lkp_crops.csv'.
- Changed 'feed_type_code' column to 'crop_code' in lkp_feeditem.csv files.
- Updated UI tab labels and mappings in params_db module.
- Automatically assign unique codes to new entries (when adding or cloning a row). Put checks to prevent having duplicates and show error.
  
2. lkp_feeditem.csv
- Deleted column de.
- Deleted feed_type_code.
- Deleted extra comma causing V8, V9 columns.
- Renamed feed_type_code to crop_code.
- Reorder columns feed_item_code followed by crop_code and feed_item_name.
- Automatically assign unique codes to new entries (when adding or cloning a row). Put checks to prevent having duplicates and show error.
  
3. lkp_livetype.csv
- Renamed column proportion_growth to proportion_growth_piglets_milk.
- Renamed column lw_gain to lw_gain_piglets.
- Renamed column cp_pregnancy to cp_lys_pregnancy.
- Renamed column cp_growth to cp_lys_growth.
- Renamed column fat_content to fat_milkcontent.
- Renamed column n_content to n_manure_content.
- Removed redudant columns:cp_lactation, milk_production, grazing_displacement_energy, cp_grazing, and live_weight_gain.
- Disabled functionalities for New Row, Clone Row and Delete.
- Disabled the livetype_code and livetype_desc from user editing.
  
4. lkp_climate.csv & lkp_climate2.csv
- Updated lkp_climate.csv with lkp_climate2.csv content.
- Deleted lkp_climate2.csv.

5. Data quality checks
- Imposed data-quality checks in the Livestock Feeding section and Area, Manure and Waste tabs in the Farm Section.

6. Crop inputs table
- Blocked editing for fertilizer columns that were not added.
- Imposed a warning above the table if the user has not selected any fertilizers earlier.
  
7. Improve parameters database tables navigation
   - Froze column names for crop_name, feed_item_name and livestock_desc to enable visibility while scrolling through long horizontal bars.
8. Help button
- updated link to the feedback form. 
---
