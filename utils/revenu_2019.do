/* OR Etat 2019 *****- REVENU -********/ 

/*cd F:\ROR\DATA\2019
local pathdata2019 F:\ROR\DATA\2019\Stata\
local pathresult2019 F:\ROR\ANALYSE\2019\resultfiles\
local pathdo2019 F:\ROR\ANALYSE\2019\dofiles\
local pathlog2019 F:\ROR\ANALYSE\2019\logfiles\ */

cd F:\ROR\ANALYSE\2019\dofiles

/****** revenu des activites principales ********/

use "F:\ROR\DATA\2019\Stata\res_m_a.dta", clear
recode a3b a3c a3e a3f (miss=0)
gen revppal= ((a3c*a3b*1000)+(a3e*a3f*1000))
collapse(sum)revppal, by(j5 year)
do dofileJ5
save "F:\ROR\ANALYSE\2019\resultfiles\revppal_2019.dta", replace


/****** revenu des activites secondaires *******/

use "F:\ROR\DATA\2019\Stata\res_as.dta", clear
recode as4 as3 as4a as3a (miss=0)
gen revsec = (as3*as4) + (as4a*as3a) 
collapse(sum)revsec, by(j5 year)
do dofileJ5
save "F:\ROR\ANALYSE\2019\resultfiles\revsec_2019.dta", replace


/****** revenu autres sources de revenu (RHA) *****/

use "F:\ROR\DATA\2019\Stata\res_rha.dta", clear
recode rha2 (miss=0)
drop rha1
rename rha2 autre_rev
do dofileJ5
save "F:\ROR\ANALYSE\2019\resultfiles\autre_rev_2019.dta", replace


/*********** revenu des exploitations **********/


/**** revenu de la riziculture ****/

* rev_riz = recette_riz - charge_riz
* recette_riz = prod_riz_val

* prod_riz

use "F:\ROR\DATA\2019\Stata\res_r.dta", clear
gen prod_riz=r23
collapse(sum)prod_riz, by(j5 year)
do dofileJ5
save "F:\ROR\ANALYSE\2019\resultfiles\prod_riz_2019.dta", replace


* prix moyen pondere du paddy

use "F:\ROR\DATA\2019\Stata\res_dc21.dta", clear
do dofileJ5
gen obs=substr(j5,1,2)
collapse(sum)dc22 dc25 if dc23==1, by(obs year)
gen pxpaddy_obs=dc25/dc22
keep obs year pxpaddy_obs
sort obs year
save "F:\ROR\ANALYSE\2019\resultfiles\px_paddy_obs_2019.dta", replace

* Evaluation monetaire de la production rizicole (Ar)

use "F:\ROR\ANALYSE\2019\resultfiles\prod_riz_2019.dta", clear
gen obs=substr(j5,1,2)
sort obs year
merge m:1 obs year using "F:\ROR\ANALYSE\2019\resultfiles\px_paddy_obs_2019.dta", nogenerate
gen prod_riz_val = prod_riz * pxpaddy_obs
drop pxpaddy_obs prod_riz obs
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\prod_riz_val_2019.dta", replace

* Recette de metayage sur les parcelles rizicoles
* Evaluation monetaire des recettes en nature

* quantite recue 
use "F:\ROR\DATA\2019\Stata\res_r.dta", clear
gen recmetloc=r6 if (r4==5 | r4==6)
collapse(sum)recmetloc, by(j5 year)
do dofileJ5
save "F:\ROR\ANALYSE\2019\resultfiles\recmetloc_2019.dta", replace

* valorisation

use "F:\ROR\ANALYSE\2019\resultfiles\recmetloc_2019.dta", clear
gen obs=substr(j5,1,2)
sort obs year
merge m:1 obs year using "F:\ROR\ANALYSE\2019\resultfiles\px_paddy_obs_2019.dta", nogenerate
gen rente_riz1 = recmetloc * pxpaddy_obs
drop pxpaddy_obs
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\rente_riz1_2019.dta", replace


* recettes en numeraire

use "F:\ROR\DATA\2019\Stata\res_r.dta", clear
gen rente_riz2=r7 if (r4==5 | r4==6)
collapse(sum)rente_riz2, by(j5 year)
do dofileJ5
save "F:\ROR\ANALYSE\2019\resultfiles\rente_riz2_2019.dta", replace


* rente riz

use "F:\ROR\ANALYSE\2019\resultfiles\rente_riz1_2019.dta", clear
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\rente_riz2_2019.dta", nogenerate
recode rente_riz1 rente_riz2 (miss=0)
gen rente_riz = rente_riz1 + rente_riz2
keep j5 year rente_riz
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\rente_riz_2019.dta", replace


* charge_riz = coutmori + coutint + coutmetloc

* depenses en main d'oeuvre non permanente: coutmori

use "F:\ROR\DATA\2019\Stata\res_mo1.dta", clear
recode mo11b mo11c mo11d mo11e mo51a mo51b mo12 mo51c mo23 (miss=0)
gen salarie= ((mo11b*mo11d) + (mo11b*mo51a) + (mo11c*mo11e) + (mo11c*mo51b))
gen tache=(mo12+mo51c)
gen entraide=mo23
gen coutmori=salarie+tache+entraide
do dofileJ5
collapse(sum)coutmori, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\coutmori_2019.dta", replace


* depenses en intrants : coutint

use "F:\ROR\DATA\2019\Stata\res_ita.dta", clear
do dofileJ5
collapse(sum)ita2, by(j5 year)
rename ita2 coutint
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\coutint_2019.dta", replace


* frais de metayage et de location en nature

use "F:\ROR\DATA\2019\Stata\res_r.dta", clear
gen rimetloc=r6 if (r4==2 | r4==3)
collapse(sum)rimetloc, by(j5 year)
do dofileJ5
save "F:\ROR\ANALYSE\2019\resultfiles\rimetloc_2019.dta", replace

use "F:\ROR\ANALYSE\2019\resultfiles\rimetloc_2019.dta", clear
gen obs=substr(j5,1,2)
sort obs year
merge m:1 obs year using "F:\ROR\ANALYSE\2019\resultfiles\px_paddy_obs_2019.dta",nogenerate
gen coutmetloc1=rimetloc*pxpaddy_obs
drop pxpaddy_obs
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\coutmetloc1_2019.dta", replace

* frais de metayage et de location en argent

use "F:\ROR\DATA\2019\Stata\res_r.dta", clear
gen coutmetloc2=r7 if (r4==2 | r4==3)
collapse(sum)coutmetloc2, by(j5 year)
do dofileJ5
save "F:\ROR\ANALYSE\2019\resultfiles\coutmetloc2_2019.dta", replace

* total frais de metayage ou de location

use "F:\ROR\ANALYSE\2019\resultfiles\coutmetloc1_2019.dta", clear
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\coutmetloc2_2019.dta", nogenerate
gen coutmetloc = coutmetloc1 + coutmetloc2
keep j5 year coutmetloc
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\coutmetloc_2019.dta", replace


* calcul du revenu procure par le riz

use "F:\ROR\ANALYSE\2019\resultfiles\prod_riz_val_2019.dta", clear
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\coutmori_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\coutint_2019", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\coutmetloc_2019", nogenerate
sort j5 year

* a la difference des calculs anterieurs, on ne considere plus la rente ici

recode  prod_riz prod_riz_val coutmori coutint coutmetloc (miss=0)
gen recette_riz= prod_riz_val
gen charge_riz= coutmori+ coutint+ coutmetloc
gen rev_riz= recette_riz- charge_riz
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\rev_riz_2019.dta", replace


/********** revenu genere par les autres cultures ********/

/**** recettes *****/

* valorisation de la production

use "F:\ROR\DATA\2019\Stata\res_c.dta", clear
do dofileJ5
egen cult=concat(c1 c37)
collapse(sum)c2, by( j5 cult year)
save "F:\ROR\ANALYSE\2019\resultfiles\prodcu_2019.dta", replace

* prix moyen pondere par forme de culture

use "F:\ROR\DATA\2019\Stata\res_c.dta", clear
do dofileJ5
gen obs=substr(j5,1,2)
egen cult=concat(c1 c37)
collapse(sum)c4 c6b, by(cult obs year)
gen prix_cu=c6b/c4
drop c4 c6b
sort cult obs year
save "F:\ROR\ANALYSE\2019\resultfiles\prix_cu_2019.dta", replace

use "F:\ROR\ANALYSE\2019\resultfiles\prodcu_2019.dta", clear
gen obs=substr(j5,1,2)
sort cult obs year
merge m:1 cult obs year using "F:\ROR\ANALYSE\2019\resultfiles\prix_cu_2019.dta", nogenerate
gen prodcu_val = c2*prix_cu
collapse(sum)prodcu_val, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\prodcu_val_2019.dta", replace


/***** charges *****/

* depenses en main d'oeuvre non permanente: coutmocu

use "F:\ROR\DATA\2019\Stata\res_mo3.dta", clear
do dofileJ5
recode mo31c mo31e mo61a mo32 mo61c mo44 (miss=0)
gen salarie=(mo31c*mo31e) + (mo31c * mo61a)
gen tache=(mo32+mo61c)
gen entraide=mo44
gen coutmocu=salarie+tache+entraide
collapse(sum)coutmocu, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\coutmocu_2019.dta", replace


* depenses en intrants

use "F:\ROR\DATA\2019\Stata\res_itb.dta", clear
do dofileJ5
rename itb5 coutintcu
collapse(sum)coutintcu, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\coutintcu_2019.dta", replace

* frais de metayage et de location en nature

use "F:\ROR\DATA\2019\Stata\res_c.dta", clear
do dofileJ5
gen obs=substr(j5,1,2)
egen cult=concat(c1 c37)
gen cumetloc=c3a
collapse(sum)cumetloc, by(j5 cult year)
save "F:\ROR\ANALYSE\2019\resultfiles\cumetloc_2019.dta", replace

use "F:\ROR\ANALYSE\2019\resultfiles\cumetloc_2019.dta", clear
gen obs=substr(j5,1,2)
sort cult obs year
merge m:1 cult obs year using "F:\ROR\ANALYSE\2019\resultfiles\prix_cu_2019.dta", nogenerate
gen coutloccu1=cumetloc*prix_cu
collapse(sum)coutloccu1, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\coutloccu1_2019.dta", replace

* frais de metayage et de location en argent

use "F:\ROR\DATA\2019\Stata\res_c.dta", clear
do dofileJ5
gen coutloccu2=c3b
collapse(sum)coutloccu2, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\coutloccu2_2019.dta", replace

* total frais de metayage
use "F:\ROR\ANALYSE\2019\resultfiles\coutloccu1_2019.dta", clear
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\coutloccu2_2019.dta", nogenerate
gen coutloccu = coutloccu1 + coutloccu2
keep j5 year coutloccu
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\coutloccu_2019.dta", replace


* rente fonciere 1 (argent recu par la mise en metayage ou la mise en location des parcelles faisant l'objet d'autres cultures)

use "F:\ROR\DATA\2019\Stata\res_fr21.dta", clear
do dofileJ5
gen rente_cu1= fr21g if fr21c==7 & fr21a!=1
collapse(sum)rente_cu1, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\rente_cu1_2019.dta", replace

* rente fonciere 2 (quantite recue par la mise en metayage ou la mise en location des parcelles faisant l'objet d'autres cultures)
use "F:\ROR\DATA\2019\Stata\res_c.dta", clear
do dofileJ5
gen obs=substr(j5,1,2)
egen cult=concat(c1 c37)
gen recmetloccu=c21
collapse(sum)recmetloccu, by(j5 cult year)
sort j5 cult year
save "F:\ROR\ANALYSE\2019\resultfiles\recmetloccu_2019.dta", replace

use "F:\ROR\ANALYSE\2019\resultfiles\recmetloccu_2019.dta", clear
gen obs=substr(j5,1,2)
sort cult obs year
merge m:1 cult obs year using "F:\ROR\ANALYSE\2019\resultfiles\prix_cu_2019.dta", nogenerate
gen rente_cu2=recmetloccu*prix_cu
collapse(sum)rente_cu2, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\rente_cu2_2019.dta", replace

use "F:\ROR\ANALYSE\2019\resultfiles\rente_cu1_2019.dta", clear
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\rente_cu2_2019.dta", nogenerate
gen rente_cu=rente_cu1 + rente_cu2
keep j5 year rente_cu
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\rente_cu_2019.dta", replace


* calcul du revenu genere par les autres cultures

use "F:\ROR\ANALYSE\2019\resultfiles\prodcu_val_2019.dta", clear
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\coutmocu_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 using "F:\ROR\ANALYSE\2019\resultfiles\coutintcu_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\coutloccu_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 using "F:\ROR\ANALYSE\2019\resultfiles\coutloccu2_2019.dta", nogenerate

recode  prodcu_val coutmocu coutintcu coutloccu coutloccu2 (miss=0)

gen recette_cu= prodcu_val
gen charge_cu=coutmocu + coutintcu + coutloccu + coutloccu2
* rente a ne pas considerer ici
gen rev_cu= recette_cu - charge_cu
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\rev_cu_2019.dta", replace


/********** revenu genere par l'elevage ********/

/**** recettes *****/

* recettes issues des ventes d'animaux d'elevage (vente cheptel bovin dans revenus exceptionnels)

use "F:\ROR\DATA\2019\Stata\res_ele.dta", clear
do dofileJ5
gen vente_bovin=ele_i2 if ele_lig==1 | ele_lig==2 | ele_lig==3
gen vente_otrani=ele_i2 if ele_lig!=1 & ele_lig!=2 & ele_lig!=3
collapse(sum) vente_bovin vente_otrani, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\vente_ani_2019.dta", replace

* recettes issues des ventes de produits d'elevage (zebu a discriminer)

* menages ayant autoconsomme du zebu

use "F:\ROR\DATA\2019\Stata\res_ele.dta", clear
do dofileJ5
gen conso_zebu=1 if (ele_lig==1 & ele_e!=0 & ele_e!=.) | (ele_lig==2 & ele_e!=0 & ele_e!=.) |(ele_lig==3 & ele_e!=0 & ele_e!=.)
collapse(sum)conso_zebu, by(j5 year)
replace conso_zebu=1 if conso_zebu>0
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\conso_zebu_2019.dta", replace

use "F:\ROR\DATA\2019\Stata\res_pe.dta", clear
do dofileJ5
sort j5 year
merge m:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\conso_zebu_2019.dta", nogenerate

gen vente_lait=pe_e*pe_c if pe_lig==1
gen vente_oeuf=pe_e*pe_c if pe_lig==2
gen vente_beef=pe_e*pe_c if pe_lig==3
gen vente_meat=pe_e*pe_c if pe_lig==4 | pe_lig==6 | pe_lig==7 
collapse(sum) vente_lait vente_oeuf vente_beef vente_meat, by(j5 year)
gen vente_nobeef = vente_lait + vente_oeuf + vente_meat
keep j5 year vente_beef vente_nobeef
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\vente_prodel_2019.dta", replace

* valorisation des autoconsommations en animaux d'elevage: (coqs/poule, 
*canards, oies, dinde � considerer dans  le revvenu agricole)

* quantite autoconsommee

use "F:\ROR\DATA\2019\Stata\res_ele.dta", clear
do dofileJ5
gen conso_ani = ele_e
collapse(sum)conso_ani, by(j5 ele_lig year)
save "F:\ROR\ANALYSE\2019\resultfiles\conso_ani_2019.dta", replace

* prix des animaux

use "F:\ROR\DATA\2019\Stata\res_ele.dta", clear
do dofileJ5
gen vte_ani=ele_i1
gen val_vte=ele_i2
gen obs=substr(j5,1,2)
collapse(sum)vte_ani val_vte, by(obs ele_lig year)
gen prix_ani=val_vte/vte_ani
keep obs year ele_lig prix_ani
sort ele_lig obs year
save "F:\ROR\ANALYSE\2019\resultfiles\prix_ani_2019.dta", replace


use "F:\ROR\ANALYSE\2019\resultfiles\conso_ani_2019.dta", clear
gen obs=substr(j5,1,2)
sort ele_lig obs year
merge ele_lig obs year using "F:\ROR\ANALYSE\2019\resultfiles\prix_ani_2019.dta"
gen conso_ani_val = conso_ani * prix_ani
collapse(sum)conso_ani_val if ele_lig==6 | ele_lig==7 | ele_lig==8 | ele_lig==9, by(j5 year)
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\conso_ani_val_2019.dta", replace

/**** charges *****/

* consommations intermediaires

use "F:\ROR\DATA\2019\Stata\res_cie.dta", clear
do dofileJ5
rename cie2 coutintel
collapse(sum)coutintel, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\coutintel_2019.dta", replace

* achat d'animaux d'elevage autres que les zebus

use "F:\ROR\DATA\2019\Stata\res_ele.dta", clear
do dofileJ5
gen achanimoval=ele_l2 if ele_lig!=1 & ele_lig!=2 & ele_lig!=3
gen achzebuval=ele_l2 if ele_lig==1 | ele_lig==2 | ele_lig==3
collapse (sum) achanimoval achzebuval, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\achanimoval_2019.dta", replace


* revenu de l'elevage: merging files

use "F:\ROR\ANALYSE\2019\resultfiles\vente_ani_2019.dta", clear
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\vente_prodel_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\conso_ani_val_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\coutintel_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\achanimoval_2019.dta", nogenerate
merge 1:1 j5 year using "F:\ROR\DATA\2019\Stata\res_el2.dta", nogenerate
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\menage_2019.dta", nogenerate

recode vente_otrani vente_nobeef conso_ani_val coutintel achanimoval (miss=0)
gen recette_el = vente_otrani + vente_nobeef + conso_ani_val
gen charge_el = coutintel + achanimoval
gen revel =  recette_el - charge_el
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\revel_2019.dta", replace

/********** revenu genere par la peche *************/

use "F:\ROR\DATA\2019\Stata\res_ppec.dta", clear
do dofileJ5
* prix moyen pondere des produits de la peche
collapse(sum)ppec_i ppec_l, by(ppec_lig year)
gen prix_peche=ppec_l/ppec_i
sort ppec_lig year
save "F:\ROR\ANALYSE\2019\resultfiles\prix_peche_2019.dta", replace

use "F:\ROR\DATA\2019\Stata\res_ppec.dta", clear
do dofileJ5
sort ppec_lig
merge m:1 ppec_lig using "F:\ROR\ANALYSE\2019\resultfiles\prix_peche_2019.dta", nogenerate
* changement de calcul par rapport aux calculs precedents: valorisation de la production
gen recette_peche = ppec_f * prix_peche  
collapse(sum)recette_peche, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\recette_peche_2019.dta", replace

 * les charges liees aux activites de peche
 * charges =  consommations intermediaires  + depenses en main d'oeuvre

* consommations intermediaires

use "F:\ROR\DATA\2019\Stata\res_cipec.dta", clear
do dofileJ5
rename cipec2 depeche1
collapse(sum)depeche1, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\depeche1_2019.dta", replace

* main d'oeuvre

use "F:\ROR\DATA\2019\Stata\res_mopec.dta", clear
do dofileJ5
collapse(sum)mopec_4 mopec_5, by(j5 year)
gen depeche2 = mopec_4 + mopec_5
drop mopec_4 mopec_5
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\depeche2_2019.dta", replace

* revenu de la peche

use "F:\ROR\ANALYSE\2019\resultfiles\recette_peche_2019.dta", clear
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\depeche1_2019.dta", nogenerate
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\depeche2_2019.dta", nogenerate
gen revpeche = recette_peche - (depeche1 + depeche2)
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\revpeche_2019.dta", replace


/******* revenu exceptionnel *******/ 

* HIMO = ACT/VCT

use "F:\ROR\DATA\2019\Stata\res_m_a.dta", clear
do dofileJ5
gen act=a4b*1000
gen vct=a4d*1000
gen himo = act + vct
collapse(sum)himo, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\himo_2019.dta", replace

* les transferts (recus et cedes)

* transferts recus
use "F:\ROR\DATA\2019\Stata\res_t1.dta", clear
do dofileJ5
	* transferts monetaires
gen transrecmo = t11d if t11a==99
	* transferts non monetaires
gen transrecnomo=t11d if t11a!=99
collapse(sum)transrecmo transrecnomo, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\transrec_2019.dta", replace

* transferts cedes
use "F:\ROR\DATA\2019\Stata\res_t2.dta", clear
do dofileJ5
	* transferts monetaires
gen transcedmo = t21d if t21a==99
	* transferts non monetaires
gen transcednomo=t21d if t21a!=99
collapse(sum)transcedmo transcednomo, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\transced_2019.dta", replace

* Vente de parcelles
use "F:\ROR\DATA\2019\Stata\res_fr21.dta", clear
do dofileJ5
gen vte_par = fr21g if fr21c==1 | fr21c==20
collapse(sum)vte_par, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\vte_par_2019.dta", replace

* Vente de zebus
	* vente_bovin dans vente_ani_2019.dta
	* vente_beef dans vente_prodel_2019.dta
	
* vente de biens de consommation et d'equipements agricoles
use "F:\ROR\DATA\2019\Stata\res_vb.dta", clear
do dofileJ5
recode vb2a vb2b vb2c vb2d vb2e vb2f vb2g (miss=0)
gen vte_biens= vb2a + vb2c + vb2d + vb2e + vb2f + vb2g  
gen vte_equip= vb2b  
keep j5 year vte_biens vte_equip
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\vte_biens_2019.dta", replace

* Autres charges d'exploitation (main d'oeuvre permanente)

use "F:\ROR\DATA\2019\Stata\res_mp1.dta", clear
do dofileJ5
gen mo_perm_mo = mp4a
gen mo_perm_nomo = mp4b
collapse (sum) mo_perm_mo mo_perm_nomo, by(j5 year)
save "F:\ROR\ANALYSE\2019\resultfiles\mo_perm_2019.dta", replace


/******** Calcul du revenu global ********/


* merging_files

use "F:\ROR\ANALYSE\2019\resultfiles\revppal_2019.dta", clear
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\revsec_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 using "F:\ROR\ANALYSE\2019\resultfiles\autre_rev_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\rente_riz_2019.dta", nogenerate
sort j5 year 
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\rev_riz_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\rev_cu_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\rente_cu_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\revel_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\revpeche_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\himo_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\transrec_2019", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\vte_par_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 using "F:\ROR\ANALYSE\2019\resultfiles\vte_biens_2019.dta", nogenerate
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\mo_perm_2019.dta", nogenerate

gen revcou = revppal + revsec + rev_riz + rev_cu + revel + revpeche
gen decap = vte_par + vente_bovin + vente_beef + vte_equip
gen revexcept = rente_riz + rente_cu + autre_rev + himo + decap + vte_biens + transrecmo + transrecnomo
gen revtot = revcou + revexcept
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\income_2019.dta", replace


* creation fichier menage

use "F:\ROR\DATA\2019\Stata\res_m_a.dta", clear
do dofileJ5
gen i=1 if m2a!=2 & m2a!=5
gen sexCM=1 if m6==1 & m4==1
replace sexCM=2 if m6==1 & m4==2
collapse(sum)i sexCM, by(j5 year)
rename i taille
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\chef_menage_2019.dta", replace

use "F:\ROR\DATA\2019\Stata\res_deb.dta", clear
* do dofileJ5
sort j5 year
merge 1:1 j5 year using "F:\ROR\ANALYSE\2019\resultfiles\chef_menage_2019.dta"
keep j5 year j0 j42 j4 taille sexCM
sort j5 year
save "F:\ROR\ANALYSE\2019\resultfiles\menage_2019.dta", replace




* modele commande
 * table obs year, contents(mean rev_riz ) format(%08.2fc)
 
 