# THIS FILE CONSTAINS THE MAPPING FOR ALL COLUMNS WITH THE RAW DATA AND THE RESPECTIVE INFORMATION
# THIS MAPPING INFORMATION IS AVAILABLE IN THE SOURCE FOLDER - DATA DESCRIPTION FILE

ms_sub_class = {'20' : '1-STORY 1946 & NEWER ALL STYLES',
				'30' : '1-STORY 1945 & OLDER',
				'40' : '1-STORY W/FINISHED ATTIC ALL AGES',
				'45' : '1-1/2 STORY - UNFINISHED ALL AGES',
				'50' : '1-1/2 STORY FINISHED ALL AGES',
				'60' : '2-STORY 1946 & NEWER',
				'70' : '2-STORY 1945 & OLDER',
				'75' : '2-1/2 STORY ALL AGES',
				'80' : 'SPLIT OR MULTI-LEVEL',
				'85' : 'SPLIT FOYER',
				'90' : 'DUPLEX - ALL STYLES AND AGES',
				'120' : '1-STORY PUD (Planned Unit Development) - 1946 & NEWER',
				'150' : '1-1/2 STORY PUD - ALL AGES',
				'160' : '2-STORY PUD - 1946 & NEWER',
				'180' : 'PUD - MULTILEVEL - INCL SPLIT LEV/FOYER',
				'190' : '2 FAMILY CONVERSION - ALL STYLES AND AGES'}

ms_zoning = {'A' : 'Agriculture',
			'C' : 'Commercial',
			'FV' : 'Floating Village Residential',
			'I' : 'Industrial',
			'RH' : 'Residential High Density',
			'RL' : 'Residential Low Density',
			'RP' : 'Residential Low Density Park',
			'RM' : 'Residential Medium Density'}

street = {"Grvl": "Gravel", "Pave" : "Paved"}

alley = {"Grvl": "Gravel", "Pave" : "Paved", "NA":"No Alley Access"}

lot_shape = {"Reg": "Regular", "IR1": "Slightly Irregular", "IR2": "Moderately Irregular", "IR3":"Irregular"}

land_countour = {"Lvl": "Near Flat / Level", "Bnk":"Banked", "HLS":"Hill Side", "Low":"Depression"}

utilities = {"AllPub":"Electricity, Gas, Water, Septic", "NoSwer":"Electricity, Gas, Water", "NoSeWa":"Electricity, Gas", "ELO":"Electric Only"}

