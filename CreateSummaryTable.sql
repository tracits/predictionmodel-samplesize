#Change all 201X to the year 2012, 2013 or 2014
CREATE TABLE NTDB_adam.201X_summary 

SELECT E.INC_KEY
	#Event equals "Died within 30 days in ED" OR "Died within 30 days Discharged"
	, CASE WHEN 
		((E.EDDISP = 'Died' OR E.EDDISP = 'Died/Expired') AND E.EDDAYS <= 30)
		OR (D.HOSPDISP = 'Expired' AND D.LOSDAYS <= 30)
	THEN 1 ELSE 0 END Event
    #Get Vitals (with VSTYPE=ED)
    , VED.SBP, VED.PULSE, VED.RR, VED.GCSTOT
FROM 
	NTDB_adam.201X_ed E
	LEFT JOIN NTDB_adam.201X_discharge D ON E.INC_KEY = D.INC_KEY
	LEFT JOIN NTDB_adam.201X_vitals VED ON E.INC_KEY = VED.INC_KEY AND VED.VSTYPE = 'ED'
#Exclude
WHERE NOT (E.SIGNSOFLIFE ='Arrived with NO signs of life' OR VED.SBP < 0 OR VED.PULSE < 0 or VED.RR < 0 or VED.GCSTOT < 0)