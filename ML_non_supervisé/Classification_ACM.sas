/* ==============================================================================
   SAE 4-02 | BUT Science des Données – Université d'Avignon
   Analyse Multivariée : Forza Horizon Cars
   Version SAS complète
   ============================================================================== */
  
/* ==============================================================================
   PARTIE 1 : CLASSIFICATION
   ============================================================================== */

/* =========================
   1. IMPORTATION
   ========================= */

/* Importation du fichier CSV brut depuis le répertoire SAS Studio */
proc import datafile="/home/u64327617/sas_reporting_multivarée_sas/Forza_Horizon_Cars.csv"
    out=car
    dbms=csv
    replace;
    guessingrows=max; /* SAS lit toutes les lignes pour deviner les types */
    getnames=yes;     /* La première ligne contient les noms de colonnes */
run;

/* Affichage des 10 premières observations pour vérification visuelle */
proc print data=car(obs=10);
run;

/* Inspection de la structure : types, longueurs, formats de chaque variable */
proc contents data=car;
run;

/* =========================
   2. NETTOYAGE
   ========================= */
  
/* --- 2.1 Remplacement info_not_found par missing --- */
/* On parcourt toutes les variables caractères avec un array
   et on remplace "info_not_found" par une chaîne vide (= missing SAS) */
data car_clean;
    set car;

    array chars{*} _character_;
    do i = 1 to dim(chars);
        if chars{i} = "info_not_found" then chars{i} = "";
    end;
    drop i; /* On supprime la variable de boucle temporaire */
run;
  
/* --- 2.2 Suppression des unités + conversion numérique --- */
/* Les variables numériques sont stockées en texte avec des unités
   (ex: "155.5 Mph", "6.2s", "0.90 g"). On les nettoie et convertit. */
data car_clean;
    set car_clean;

    /* compress() supprime les virgules (séparateurs de milliers) 
       puis input() convertit la chaîne en numérique */
    In_Game_Price_num = input(compress(In_Game_Price, ","),  best32.);
    Weight_lbs_num    = input(compress(Weight_lbs,    ","),  best32.);

    /* scan(..., 1, " ") extrait le premier mot avant l'espace
       → supprime " Mph" et " g" */
    Top_Speed_num     = input(scan(Top_Speed,       1, " "), best32.);
    g_force_num       = input(scan('g-force'n,      1, " "), best32.);

    /* scan(..., 1, "s") extrait la partie avant le "s" 
       → supprime le "s" des secondes (ex: "6.2s" → 6.2) */
    X0_60_Mph_num     = input(scan('0-60_Mph'n,    1, "s"), best32.);
    X0_100_Mph_num    = input(scan('0-100_Mph'n,   1, "s"), best32.);

    /* input() avec best32. convertit directement les variables 
       numériques stockées en texte sans unités */
    speed_num        = input(speed,        best32.);
    handling_num     = input(handling,     best32.);
    acceleration_num = input(acceleration, best32.);
    launch_num       = input(launch,       best32.);
    braking_num      = input(braking,      best32.);
    Offroad_num      = input(Offroad,      best32.);
    Stock_Rating_num = input(Stock_Rating, best32.);
    Horse_Power_num  = input(Horse_Power,  best32.);

    /* On supprime les anciennes versions texte des variables
       Les noms avec tirets nécessitent la syntaxe 'nom'n */
    drop In_Game_Price Weight_lbs Top_Speed 'g-force'n '0-60_Mph'n '0-100_Mph'n
         speed handling acceleration launch braking Offroad Stock_Rating Horse_Power;

    /* On redonne les noms d'origine aux nouvelles variables numériques */
    rename
        In_Game_Price_num = In_Game_Price
        Weight_lbs_num    = Weight_lbs
        Top_Speed_num     = Top_Speed
        X0_60_Mph_num     = X0_60_Mph
        X0_100_Mph_num    = X0_100_Mph
        g_force_num       = g_force
        speed_num         = speed
        handling_num      = handling
        acceleration_num  = acceleration
        launch_num        = launch
        braking_num       = braking
        Offroad_num       = Offroad
        Stock_Rating_num  = Stock_Rating
        Horse_Power_num   = Horse_Power;
run;

/* =========================
   Vérification des types de variables 
   ========================= */
/* On vérifie que toutes les conversions ont bien produit des variables 
   numériques et non plus des variables caractères */
proc contents data=car_clean;
run;

/* =========================
   3. ANALYSE NA
   ========================= */

/* Comptage des valeurs manquantes pour toutes les variables numériques
   N = nombre d'observations non manquantes
   NMISS = nombre de valeurs manquantes */
proc means data=car_clean nmiss n mean std;
run;

/* Comptage des valeurs manquantes pour toutes les variables qualitatives */

/* ÉTAPE 1 : On crée un dataset long avec une ligne par variable par observation
   vname() récupère le nom de la variable courante dans l'array */
data miss_char;
    set car_clean;
    array chars{*} _character_;
    do i = 1 to dim(chars);
        varname = vname(chars{i});
        n_miss  = 0;
        if missing(chars{i}) then n_miss = 1;
        output;
    end;
    keep varname n_miss;
run;

/* ÉTAPE 2 : On agrège par variable et on compte les manquants
   Trié par ordre décroissant pour voir les variables les plus incomplètes en premier */
proc sql;
    select varname     as Variable,
           sum(n_miss) as N_Manquants
    from miss_char
    group by varname
    having sum(n_miss) >= 0
    order by N_Manquants descending;
quit;

/* =========================
   4. SUPPRESSION VARIABLES + LIGNES
   ========================= */

/* On supprime :
   - car_source_2 : trop de valeurs manquantes (416/539)
   - X0_100_Mph, X0_60_Mph, Top_Speed, g_force : 399 manquants chacune,
     trop peu exploitables pour l'analyse
   - Les lignes sans Model_type ou Drive_Type : variables critiques pour l'ACM */
data car_clean2;
    set car_clean;

    drop car_source_2 X0_100_Mph X0_60_Mph Top_Speed g_force;

    if missing(Model_type) then delete;
    if missing(Drive_Type) then delete;
run;

/* =========================
   5. YEAR + MODEL
   ========================= */

/* On extrait l'année (4 premiers caractères) et le modèle (à partir du 6ème)
   depuis la variable Name_and_model (ex: "2019 Ferrari 812 Superfast")
   input() avec format 4. convertit les 4 caractères en entier numérique */
data car_clean2;
    set car_clean2;

    Year  = input(substr(Name_and_model, 1, 4), 4.);
    Model = substr(Name_and_model, 6);

    drop Name_and_model; /* La variable source n'est plus nécessaire */
run;

/* =========================
   6. ANALYSE DESCRIPTIVE
   ========================= */

/* Statistiques descriptives classiques pour toutes les variables numériques */
proc means data=car_clean2 n mean median std min max;
run;

/* Histogrammes avec courbe normale superposée pour détecter 
   les asymétries et valeurs aberrantes avant imputation */
proc univariate data=car_clean2;
    var Horse_Power In_Game_Price speed handling acceleration launch braking Offroad;
    histogram / normal;
run;

/* =========================
   7. IMPUTATION MÉDIANE
   ========================= */

/* On impute les valeurs manquantes par la médiane de chaque variable
   method=median : remplace les NA par la médiane
   reponly : ne remplace que les manquants, ne modifie pas les autres valeurs */
proc stdize data=car_clean2 out=car_clean3 method=median reponly;
    var Horse_Power Stock_Rating speed handling acceleration launch braking 
        Offroad In_Game_Price Weight_lbs;
run;

/* =========================
   8. VÉRIFICATION NA
   ========================= */

/* On vérifie qu'il ne reste plus aucune valeur manquante après imputation */
proc means data=car_clean3 nmiss;
run;

/* =========================
   9. ANALYSE FINALE
   ========================= */

/* Statistiques descriptives finales après nettoyage et imputation */
proc means data=car_clean3;
run;

/* Histogrammes finaux pour vérifier les distributions après imputation */
proc univariate data=car_clean3;
    var Horse_Power In_Game_Price;
    histogram;
run;

/* Suppression des doublons exacts sur toutes les variables
   NODUPREC : supprime les lignes entièrement dupliquées
   Le nombre de doublons supprimés apparaît dans le Log SAS */
proc sort data=car_clean3 out=df_propre noduprec;
    by _all_;
run;

/* ── 4. DÉTECTION DES OUTLIERS (Règle IQR de Tukey) ────────────────────── */

/* Affichage des 5 valeurs les plus hautes et les plus basses
   pour détecter visuellement les valeurs aberrantes extrêmes */
proc univariate data=car_clean3;
    var Horse_Power In_Game_Price Weight_lbs speed;
    ods select ExtremeObs;
    title "5 valeurs extremes (hautes et basses) par variable";
run;

/* Calcul des quartiles Q1 et Q3 pour toutes les variables
   autoname génère automatiquement les noms Q1_VarName et Q3_VarName */
proc means data=car_clean3 noprint;
    var Horse_Power Stock_Rating speed handling acceleration launch braking 
        Offroad In_Game_Price Weight_lbs;
    output out=stats Q1= Q3= / autoname;
run;

/* Détection des outliers par la règle IQR de Tukey :
   Une valeur est aberrante si elle est en dehors de
   [Q1 - 1.5*IQR ; Q3 + 1.5*IQR]
   On utilise IF _N_ = 1 THEN SET stats pour lire les quartiles
   une seule fois et les garder en mémoire pour toutes les observations */
data outliers_IQR;
    set car_clean3;
    if _N_ = 1 then set stats;
    array vars    {*} Horse_Power Stock_Rating speed handling acceleration 
                      launch braking Offroad In_Game_Price Weight_lbs;
    array Q1s     {*} Q1_:;  /* Tous les Q1 générés par autoname */
    array Q3s     {*} Q3_:;  /* Tous les Q3 générés par autoname */
    array IQRs    {*} IQR_:; /* Écarts interquartiles calculés */
    array lower   {*} Lower_:; /* Seuils inférieurs */
    array upper   {*} Upper_:; /* Seuils supérieurs */
    array outlier_flags {*} Outlier_:; /* 1 si outlier, 0 sinon */

    do i = 1 to dim(vars);
        IQRs[i]  = Q3s[i] - Q1s[i];
        lower[i] = Q1s[i] - 1.5 * IQRs[i];
        upper[i] = Q3s[i] + 1.5 * IQRs[i];
        if vars[i] < lower[i] or vars[i] > upper[i] 
            then outlier_flags[i] = 1; /* Valeur aberrante */
        else outlier_flags[i] = 0;     /* Valeur normale */
    end;
    drop i _type_ _freq_;
run;
/* Résultat : aucune valeur aberrante détectée → pas de suppression nécessaire
   La classification peut être réalisée sur l'ensemble des données */

/* Sélection des variables numériques uniquement pour la normalisation
   keep _numeric_ garde toutes les variables numériques
   On retire Year car c'est un identifiant temporel, pas une mesure de performance */
data car_quant;
    set car_clean3;
    keep _numeric_;
    drop Year;
run;

/* Normalisation (centrage-réduction) : moyenne=0, écart-type=1
   Indispensable avant la CAH pour éviter que les variables avec 
   de grandes échelles (ex: Horse_Power en centaines) dominent 
   celles avec de petites échelles (ex: speed de 1 à 10) */
proc standard data=car_quant mean=0 std=1 out=car_normalized;
    var _numeric_;
run;

/* -------------------------------------------------------------------------- */
/* CLASSIFICATION ASCENDANTE HIÉRARCHIQUE - MÉTHODE DE WARD                  */
/* -------------------------------------------------------------------------- */

/* Premier proc cluster sans identifiant pour avoir le tableau des 15 derniers
   niveaux de fusion (print=15) et vérifier la structure générale */
proc cluster data=car_normalized
             method=ward   /* Critère de Ward : minimise la variance intra-classe */
             outtree=tree  /* Sauvegarde l'arbre hiérarchique dans tree */
             standard      /* Standardise les données en interne (sécurité) */
             nonorm        /* Pas de normalisation supplémentaire de l'inertie */
             print=15;     /* Affiche les 15 derniers niveaux de fusion */
    var Horse_Power Stock_Rating speed handling acceleration launch braking 
        Offroad In_Game_Price Weight_lbs;
    copy Horse_Power Stock_Rating speed handling acceleration launch braking 
         Offroad In_Game_Price Weight_lbs; /* Copie les variables dans tree */
    title "Classification Ascendante Hiérarchique - Méthode de Ward";
run;

/* Ajout d'un identifiant unique pour pouvoir fusionner les résultats
   avec les données originales après la classification */
data car_normalized2;
    set car_normalized;
    ID_voiture = _N_; /* _N_ = numéro de ligne, unique pour chaque observation */
run;

/* Second proc cluster avec identifiant pour le dendrogramme et l'extraction
   des classes (proc tree nécessite un id pour afficher les labels) */
proc cluster data=car_normalized2
             method=ward
             outtree=tree
             standard
             nonorm;
    id ID_voiture;
    var Horse_Power Stock_Rating speed handling acceleration
        launch braking Offroad In_Game_Price Weight_lbs;
    copy Horse_Power Stock_Rating speed handling acceleration
         launch braking Offroad In_Game_Price Weight_lbs;
    title "Classification Ascendante Hiérarchique - Méthode de Ward";
run;

/* Tri obligatoire de l'arbre par _ncl_ (nombre de classes) 
   avant l'extraction de SPRSQ/RSQ pour que by _ncl_ fonctionne */
proc sort data=tree; by _ncl_; run;

/* Dendrogramme horizontal avec hauteur = nombre de classes
   level=3 limite l'affichage aux 3 derniers niveaux de fusion */
proc tree data=tree horizontal ncl=3 level=3;
    title "Dendrogramme simplifié en 3 classes de Ward";
    height _ncl_;
run; quit;

/* Dendrogramme avec hauteur = R² pour voir la proportion d'inertie
   inter-classes expliquée à chaque niveau de fusion */
proc tree data=tree horizontal lines=(color=red);
    title "Dendrogramme selon le RSQ";
    height _rsq_;
    copy Horse_Power Stock_Rating speed handling acceleration
         launch braking Offroad In_Game_Price Weight_lbs;
    id ID_voiture;
run; quit;

/* Extraction de la perte d'inertie (SPRSQ) à chaque niveau
   first._ncl_ garde une seule ligne par valeur de _ncl_ */
data SPRSQ;
    set tree;
    by _ncl_;
    if first._ncl_;
    keep _ncl_ _sprsq_;
run;

/* Extraction du R² global à chaque niveau de fusion */
data RSQ;
    set tree;
    by _ncl_;
    if first._ncl_;
    keep _ncl_ _rsq_;
run;

/* Graphique de la perte d'inertie (SPRSQ) selon le nombre de classes
   Un saut important du SPRSQ indique une bonne coupure
   On limite à 20 classes maximum pour la lisibilité */
axis1 label=("Nombre de classes");
axis2 label=(angle=90 "SPRSQ") style=1 order=(0 to 0.70 by 0.05);

proc gplot data=SPRSQ;
    title "Perte d'inertie interclasse selon le nombre de classes";
    where _ncl_ < 20 and _ncl_ > 0;
    plot _sprsq_ * _ncl_ = 1 / haxis=axis1 vaxis=axis2;
    symbol1 i=join c=red; /* i=join relie les points par une ligne */
run; quit;

/* Graphique du R² global : on cherche le coude où le gain 
   marginal devient faible (courbe qui se stabilise) */
axis1 label=("Nombre de classes");
axis2 label=(angle=90 "RSQ") style=1 order=(0 to 1 by 0.05);

proc gplot data=RSQ;
    title "Inertie interclasse selon le nombre de classes";
    where _ncl_ < 10 and _ncl_ > 0;
    plot _rsq_ * _ncl_ = 1 / haxis=axis1 vaxis=axis2;
    symbol1 i=join c=red;
run; quit;

/* ── 7. EXTRACTION DES CLASSES (k=3) ────────────────────────────────────── */

/* Découpage de l'arbre en 3 classes selon les graphiques du coude
   out=classes_cars : dataset résultat avec une variable CLUSTER par observation */
proc tree data=tree horizontal out=classes_cars
          lines=(color=red) nclusters=3 noprint;
    height _ncl_;
    copy Horse_Power Stock_Rating speed handling acceleration
         launch braking Offroad In_Game_Price Weight_lbs;
    id ID_voiture;
run; quit;

/* Tri par cluster pour afficher les observations groupe par groupe */
proc sort data=classes_cars; by cluster; run;

/* Affichage d'un extrait des 40 premières observations par classe */
proc print data=classes_cars (obs=40) heading=h;
    title "Extrait des observations par classe (k=3)";
    var speed handling acceleration launch braking Offroad
        Stock_Rating Horse_Power In_Game_Price Weight_lbs;
    id ID_voiture;
    by cluster;
run;

/* Comptage des effectifs dans chaque classe */
proc freq data=classes_cars;
    tables cluster;
    title "Repartition des voitures dans les 3 classes";
run;

/* ── 8. CARACTÉRISATION DES CLASSES - V-TESTS ───────────────────────────── */

/* Ajout de l'identifiant à car_clean3 pour pouvoir fusionner 
   avec classes_cars qui contient les numéros de cluster */
data car_clean3_id;
    set car_clean3;
    ID_voiture = _N_;
run;

/* Tri des deux tables par l'identifiant commun avant la fusion */
proc sort data=car_clean3_id; by ID_voiture; run;
proc sort data=classes_cars;  by ID_voiture; run;

/* Fusion des données originales avec les affectations de cluster
   keep=ID_voiture cluster : on ne garde que ces deux variables de classes_cars */
data cars_complete;
    merge car_clean3_id classes_cars (keep=ID_voiture cluster);
    by ID_voiture;
run;

/* Vérification visuelle de la table fusionnée */
proc print data=cars_complete (obs=20) heading=h;
    title "Extrait de la table CARS_COMPLETE";
run;

/* Statistiques globales sur l'ensemble des observations
   Servira de référence pour calculer les V-tests */
proc means data=cars_complete noprint;
    var speed handling acceleration launch braking Offroad
        Stock_Rating Horse_Power Weight_lbs In_Game_Price;
    output out=stat1; /* Contient N, MEAN, STD, MIN, MAX pour chaque variable */
run;

proc print data=stat1;
    title "Contenu de la table STAT1";
run;

/* Extraction de la taille totale N de l'échantillon
   On génère 4*10=40 lignes (4 classes × 10 variables) pour la fusion future */
data tmp1;
    set stat1;
    where (_STAT_ = "N");
    N = _FREQ_; /* _FREQ_ contient le nombre total d'observations */
    do i = 1 to 4;
        do j = 1 to 10;
            output;
        end;
    end;
    keep N;
run;

/* Extraction des moyennes et écarts-types globaux 
   Ces valeurs serviront de référence pour le calcul des V-tests */
data tmp2;
    set stat1;
    where (_STAT_ = "MEAN" or _STAT_ = "STD");
    drop _TYPE_ _FREQ_;
run;

/* Transposition : une ligne par variable avec V1=MEAN et V2=STD */
proc transpose data=tmp2 out=test1 name=_NAME_ prefix=V;
run;

proc print data=test1;
    title "Contenu de la table TEST1";
run;

/* Duplication de test1 pour 4 clusters : chaque variable doit 
   apparaître 4 fois (une fois par classe) */
data test1;
    set test1 test1 test1 test1;
    MEAN = V1; /* Moyenne globale de la variable */
    STD  = V2; /* Écart-type global de la variable */
    drop V1-V2;
run;

/* Création des numéros de cluster : 4 groupes × 10 variables = 40 lignes */
data tmp3;
    do i = 1 to 4;
        do j = 1 to 10;
            CLUSTER = i;
            output;
        end;
    end;
    drop i j;
run;

/* Fusion des statistiques globales avec les numéros de cluster */
data test1;
    merge test1 tmp1 tmp3;
run;

proc print data=test1;
    title "Contenu de la table TEST1";
run;

/* Tri par cluster avant le calcul des statistiques par groupe */
proc sort data=cars_complete; by cluster; run;

/* Statistiques descriptives par cluster 
   La clause BY cluster produit un jeu de stats pour chaque classe */
proc means data=cars_complete noprint;
    var speed handling acceleration launch braking Offroad
        Stock_Rating Horse_Power Weight_lbs In_Game_Price;
    by cluster;
    output out=stat2;
run;

proc print data=stat2;
    title "Contenu de la table STAT2";
run;

/* Extraction des effectifs par cluster
   _FREQ_ contient le nombre d'observations dans chaque classe
   On génère 10 lignes par cluster (une par variable) */
data tmp1;
    set stat2;
    where (_STAT_ = "N");
    N_CL = _FREQ_; /* Taille de la classe k */
    do i = 1 to 10;
        output;
    end;
    keep N_CL;
run;

proc print data=tmp1;
    title "Contenu de la table TMP1";
run;

/* Extraction des moyennes par cluster 
   On garde uniquement les 10 variables d'analyse */
data tmp2;
    set stat2;
    by cluster;
    where (_STAT_ = "MEAN");
    keep speed handling acceleration launch braking Offroad
         Stock_Rating Horse_Power Weight_lbs In_Game_Price;
run;

proc print data=tmp2;
    title "Contenu de la table TMP2 (1/2)";
run;

/* Transformation en format long : une ligne par variable par cluster
   MEAN_CL = moyenne de la variable dans la classe k */
data tmp2;
    set tmp2;
    array S[10] speed handling acceleration launch braking Offroad
                Stock_Rating Horse_Power Weight_lbs In_Game_Price;
    do i = 1 to 10;
        MEAN_CL = S[i];
        output;
    end;
    keep i MEAN_CL;
run;

proc print data=tmp2;
    title "Contenu de la table TMP2 (2/2)";
run;

/* Extraction des écarts-types par cluster */
data tmp3;
    set stat2;
    by cluster;
    where (_STAT_ = "STD");
    keep speed handling acceleration launch braking Offroad
         Stock_Rating Horse_Power Weight_lbs In_Game_Price;
run;

proc print data=tmp3;
    title "Contenu de la table TMP3 (1/2)";
run;

/* Transformation en format long : STD_CL = écart-type dans la classe k */
data tmp3;
    set tmp3;
    array S[10] speed handling acceleration launch braking Offroad
                Stock_Rating Horse_Power Weight_lbs In_Game_Price;
    do i = 1 to 10;
        STD_CL = S[i];
        output;
    end;
    keep i STD_CL;
run;

proc print data=tmp3;
    title "Contenu de la table TMP3 (2/2)";
run;

/* Fusion des statistiques par cluster en une seule table */
data test2;
    merge tmp1 tmp2 tmp3;
    keep N_CL MEAN_CL STD_CL;
run;

proc print data=test2;
    title "Contenu de la table TEST2";
run;

/* Fusion finale : statistiques globales + statistiques par cluster */
data test;
    merge test1 test2;
run;

proc print data=test;
    title "Contenu de la table TEST";
run;

/* Calcul du V-test pour chaque variable dans chaque classe
   num   = différence entre moyenne de la classe et moyenne globale
   denom = variance théorique sous H0 (tirage sans remise)
   V_TEST > 2 ou < -2 indique une surreprésentation ou sous-représentation
   significative de la modalité dans la classe (seuil 5%) */
/* Calcul V-test */
data test;
    set test;
    num    = MEAN_CL - MEAN;
    denom  = ((N - N_CL) / (N - 1)) * (STD * STD / N_CL);
    V_TEST = num / sqrt(denom);  /* sqrt() ajouté ici */
run;
/* Affichage des V-tests triés par classe */
proc sort data=test; by CLUSTER; run;

proc print data=test;
    title "Table des V-Tests";
    var _NAME_ CLUSTER V_TEST;
    by CLUSTER;
run;

/* ── 9. VISUALISATION DES CLASSES ───────────────────────────────────────── */

/* Boxplot de la vitesse par classe pour voir si les classes 
   se distinguent bien sur cette variable */
proc sgplot data=cars_complete;
    vbox speed / category=cluster fillattrs=(transparency=0.3)
                 outlierattrs=(symbol=circlefilled);
    xaxis label="Classe" values=(1 2 3 );
    yaxis label="Score de vitesse (speed)";
    title "Distribution du score de vitesse par classe";
run;

/* Boxplot de la puissance moteur par classe */
proc sgplot data=cars_complete;
    vbox Horse_Power / category=cluster fillattrs=(transparency=0.3)
                       outlierattrs=(symbol=circlefilled);
    xaxis label="Classe" values=(1 2 3 );
    yaxis label="Puissance (Horse_Power)";
    title "Distribution de la puissance moteur par classe";
run;

/* Boxplot du score Offroad par classe */
proc sgplot data=cars_complete;
    vbox Offroad / category=cluster fillattrs=(transparency=0.3);
    xaxis label="Classe" values=(1 2 3 );
    yaxis label="Score tout-terrain (Offroad)";
    title "Distribution du score Offroad par classe";
run;

/* Nuage de points : Puissance vs Vitesse coloré par classe
   Permet de voir la séparation géométrique des classes */
proc sgplot data=cars_complete;
    scatter x=Horse_Power y=speed / group=cluster
            markerattrs=(size=8 symbol=circlefilled);
    xaxis label="Puissance moteur (Horse_Power)";
    yaxis label="Score de vitesse (speed)";
    title "Vitesse vs Puissance - colore par classe";
    title2 "Forza Horizon Cars - CAH Ward k=3";
run;

/* Nuage de points : Accélération vs Maniabilité par classe */
proc sgplot data=cars_complete;
    scatter x=acceleration y=handling / group=cluster
            markerattrs=(size=8 symbol=squarefilled);
    xaxis label="Score d'acceleration";
    yaxis label="Score de maniabilite (handling)";
    title "Acceleration vs Maniabilite - par classe";
run;

/* Calcul des profils moyens par classe pour 7 variables de performance */
proc means data=cars_complete mean noprint;
    var speed handling acceleration launch braking Offroad Stock_Rating;
    by cluster;
    output out=profil_classes mean=;
run;

/* Transposition en format long pour le graphique en barres groupées */
proc transpose data=profil_classes out=profil_long name=Variable;
    var speed handling acceleration launch braking Offroad Stock_Rating;
    by cluster;
run;

/* Renommage de col1 en Moyenne pour la lisibilité */
data profil_long;
    set profil_long;
    rename col1 = Moyenne;
run;

/* Graphique en barres groupées : profil moyen de chaque classe
   Permet de comparer visuellement les 3 classes sur toutes les variables */
proc sgplot data=profil_long;
    vbar Variable / response=Moyenne group=cluster
                    groupdisplay=cluster filltype=gradient;
    xaxis label="Variable de performance";
    yaxis label="Score moyen" min=0 max=10;
    title "Profil moyen des 4 classes - Variables de performance";
    title2 "Forza Horizon Cars - Classification Ward";
run;

/* ── 10. CROISEMENT AVEC VARIABLES QUALITATIVES ──────────────────────────── */

/* Tableau croisé : classe × type de modèle
   Permet de voir si certains types de voitures sont concentrés dans une classe */
proc freq data=cars_complete;
    tables cluster * Model_type / norow nocol nopercent;
    title "Repartition des types de modeles par classe";
run;

/* Tableau croisé : classe × type de transmission
   Permet de voir si les voitures 4WD, RWD, FWD se regroupent différemment */
proc freq data=cars_complete;
    tables cluster * Drive_Type / norow nocol nopercent;
    title "Repartition des types de transmission par classe";
run;


/*==============================================================================
   PARTIE 2 : ANALYSE DES CORRESPONDANCES MULTIPLES (ACM)

   POURQUOI CES VARIABLES QUALITATIVES POUR L'ACM ?
   ─────────────────────────────────────────────────
   L'ACM nécessite exclusivement des variables qualitatives.
   
   HP_cat, speed_cat, rating_cat, prix_cat :
   → Les variables Horse_Power, speed, Stock_Rating et In_Game_Price sont
     quantitatives mais portent une information de PROFIL de voiture.
     On les discrétise en tranches pour les rendre utilisables en ACM.
     Exemple : "HP_4_Tres_Puiss" identifie clairement une hypercar.
   
   decennie :
   → Year brut a trop de modalités distinctes (1950 à 2022 = ~70 modalités).
     Le regroupement par décennie donne 7 modalités interprétables et
     permet d'étudier l'évolution des profils de voitures dans le temps.
   
   Drive_clean (Drive_Type) :
   → Variable qualitative native indiquant le type de transmission
     (AWD, RWD, FWD). Directement utilisable en ACM sans transformation.
     Elle structure fortement les profils : les voitures AWD sont souvent
     des SUV/tout-terrain, les RWD des sportives de circuit.
   
   Model_clean (Model_type) :
   → Variable qualitative native indiquant la catégorie de voiture
     (Muscle Car, Modern Car, Classic Sports Car, etc.).
     C'est la variable la plus discriminante pour identifier des profils
     car elle reflète directement le positionnement marketing de la voiture.
   
   VARIABLES EXCLUES DE L'ACM :
   → handling, acceleration, launch, braking, Offroad, Weight_lbs :
     déjà incluses dans la CAH. Les ajouter en ACM créerait de la redondance.
   → Manufacturer, Model : trop de modalités distinctes (centaines de marques),
     ce qui rendrait le graphique factoriel illisible.
==============================================================================*/

/* ── 1. VÉRIFICATION DE LA STRUCTURE ──────────────────────────────────── */

/* Aperçu des 5 premières observations de la table nettoyée */
proc print data=car_clean3 (obs=5);
    title "Apercu des 5 premieres observations (car_clean3)";
run;

/* Inspection des types et formats de toutes les variables */
proc contents data=car_clean3;
run;

/* ── 2. DISCRÉTISATION ET CONVERSION EN CARACTÈRES ─────────────────────── */

data cars_acm;
    set car_clean3;

    /* On fixe les longueurs en avance pour éviter la troncature des modalités
       lors de l'affectation conditionnelle (règle SAS : la longueur est fixée
       par la première affectation rencontrée si non déclarée) */
    length HP_cat speed_cat rating_cat prix_cat decennie $20. 
           Drive_clean Model_clean $50.;

    /* Discrétisation de Horse_Power en 4 tranches
       Si manquant : modalité "HP_Manquant" pour traçabilité
       Les seuils correspondent aux quartiles approximatifs observés */
    if missing(Horse_Power)        then HP_cat = "HP_Manquant";
    else if Horse_Power <= 250     then HP_cat = "HP_1_Faible";
    else if Horse_Power <= 400     then HP_cat = "HP_2_Moyen";
    else if Horse_Power <= 600     then HP_cat = "HP_3_Puissant";
    else                                HP_cat = "HP_4_Tres_Puiss";

    /* Discrétisation de speed en 3 tranches
       Scores de 1 à 10 dans Forza : lente ≤5, rapide 5-7, très rapide >7 */
    if missing(speed)              then speed_cat = "SP_Manquant";
    else if speed <= 5             then speed_cat = "SP_1_Lente";
    else if speed <= 7             then speed_cat = "SP_2_Rapide";
    else                                speed_cat = "SP_3_Tres_Rapide";

    /* Discrétisation de Stock_Rating en 3 tranches
       Note globale de 1 à 10 : bas ≤5, moyen 5-7, élevé >7 */
    if missing(Stock_Rating)       then rating_cat = "RT_Manquant";
    else if Stock_Rating <= 5      then rating_cat = "RT_1_Bas";
    else if Stock_Rating <= 7      then rating_cat = "RT_2_Moyen";
    else                                rating_cat = "RT_3_Eleve";

    /* Discrétisation de In_Game_Price en 4 tranches
       Prix en crédits du jeu : très variable de ~20k à plusieurs millions */
    if missing(In_Game_Price)          then prix_cat = "PR_Manquant";
    else if In_Game_Price <= 100000    then prix_cat = "PR_1_Accessible";
    else if In_Game_Price <= 500000    then prix_cat = "PR_2_Intermediaire";
    else if In_Game_Price <= 1000000   then prix_cat = "PR_3_Premium";
    else                                    prix_cat = "PR_4_Luxe";

    /* Regroupement de Year en décennies
       Year brut = trop de modalités → on regroupe par décennie */
    if missing(Year)               then decennie = "YR_Manquant";
    else if Year < 1960            then decennie = "YR_1_Avant1960";
    else if Year < 1970            then decennie = "YR_2_Annees60";
    else if Year < 1980            then decennie = "YR_3_Annees70";
    else if Year < 1990            then decennie = "YR_4_Annees80";
    else if Year < 2000            then decennie = "YR_5_Annees90";
    else if Year < 2010            then decennie = "YR_6_Annees2000";
    else                                decennie = "YR_7_2010plus";

    /* Nettoyage de Drive_Type : si vide → modalité explicite "DR_Manquant"
       strip() supprime les espaces en début et fin de chaîne */
    if strip(Drive_Type) = "" then Drive_clean = "DR_Manquant";
    else Drive_clean = strip(Drive_Type);

    /* Même traitement pour Model_type */
    if strip(Model_type) = "" then Model_clean = "MD_Manquant";
    else Model_clean = strip(Model_type);

    /* On ne garde que les variables qualitatives créées pour l'ACM
       Aucune variable numérique parasite ne doit rester dans ce dataset */
    keep HP_cat speed_cat rating_cat prix_cat decennie Drive_clean Model_clean;
run;

/* ── 3. VÉRIFICATION DES FRÉQUENCES ET DES VALEURS MANQUANTES ───────────── */

/* Contrôle des fréquences de chaque modalité
   missing : affiche aussi les valeurs vides
   On vérifie qu'aucune modalité n'a moins de 5 observations
   (risque d'instabilité dans l'ACM) */
proc freq data=cars_acm;
    tables HP_cat speed_cat rating_cat prix_cat decennie 
           Drive_clean Model_clean / missing;
    title "Verification des modalites actives avant l'ACM";
run;

/* ── 4. EXÉCUTION DE L'ACM (PROC CORRESP) ───────────────────────────────── */

/* mca : indique qu'on fait une ACM (Multiple Correspondence Analysis)
   dimens=2 : on retient 2 dimensions pour le plan factoriel
   outc=coord : sauvegarde les coordonnées factorielles des modalités
   short : rapport condensé, évite les tableaux trop volumineux */
proc corresp data=cars_acm mca dimens=2 outc=coord short;
    tables HP_cat speed_cat rating_cat prix_cat decennie Drive_clean Model_clean;
    title "ACM - Analyse des Correspondances Multiples (Forza Horizon)";
run;

/* ── 5. VÉRIFICATION ET EXPLORATION DU DATASET DE SORTIE ────────────────── */

/* On vérifie les types de lignes dans coord :
   VAR = modalités actives (coordonnées calculées par l'ACM)
   SUPVAR = modalités supplémentaires (si on en avait défini)
   INERTIA = inertie par axe */
proc freq data=coord;
    tables _type_;
    title "Structure du fichier de sortie coord de la PROC CORRESP";
run;

/* Affichage des coordonnées factorielles des 100 premières modalités
   _name_ = nom de la modalité
   dim1, dim2 = coordonnées sur les axes 1 et 2 */
proc print data=coord (obs=100);
    var _name_ _type_ dim1 dim2;
    title "Coordonnes factorielles des modalites (100 premieres obs)";
run;

/* ── 6. GRAPHIQUE FACTORIEL DE L'ACM ────────────────────────────────────── */

/* On filtre sur 'VAR' uniquement car pas de variables supplémentaires
   datalabel=_name_ : affiche le nom de la modalité sur le graphique
   Les modalités proches sur le plan sont fréquemment associées */
proc sgplot data=coord;
    where _type_ = 'VAR';
    scatter x=dim1 y=dim2 / datalabel=_name_;
    xaxis label="Axe 1";
    yaxis label="Axe 2";
    title "Graphique factoriel de l'ACM (Axes 1 et 2) - Profils des Modalites";
run;

/* ── 7. PRÉPARATION DES MODALITÉS ACTIVES POUR LE CLUSTERING ────────────── */

/* On isole strictement les modalités actives (VAR)
   pour ne classifier que celles-ci, pas les lignes d'inertie */
data coord_var;
    set coord;
    where _type_ = 'VAR';
run;

/* ── 8. CLASSIFICATION HIÉRARCHIQUE DE WARD (CAH) ───────────────────────── */

/* CAH sur les coordonnées factorielles dim1 et dim2
   pseudo, ccc, rsquare : indicateurs pour choisir le nombre de classes
   id _name_ : identifiant = nom de la modalité */
proc cluster data=coord_var method=ward outtree=tree_acm pseudo ccc rsquare;
    id _name_;
    var dim1 dim2;
    title "CAH de Ward sur les modalites actives de l'ACM";
run;

/* ── 9. DENDROGRAMME DES MODALITÉS ACTIVES ──────────────────────────────── */

/* Rendu haute résolution pour que les noms des modalités soient lisibles
   imagefmt=png width=1500px height=800px : grande image pour éviter 
   les chevauchements de labels */
ods graphics on / imagefmt=png width=1500px height=800px reset=all;

/* Dendrogramme vertical avec hauteur = R²
   Permet de voir quelles modalités se regroupent en premier */
proc tree data=tree_acm;
    id _name_;
    height _rsq_;
    title h=14pt "Dendrogramme des modalites actives - Methode de Ward";
    title2 h=10pt "Forza Horizon Cars - Rendu Graphique Haute Resolution";
run; quit;

ods graphics off; /* Retour au mode graphique normal */

/* ── 10. GRAPHIQUE DU COUDE ─────────────────────────────────────────────── */

/* Tri obligatoire avant l'extraction par first._ncl_ */
proc sort data=tree_acm; by _ncl_; run;

/* Extraction d'une ligne par niveau de fusion
   first._ncl_ : garde la première occurrence de chaque valeur de _ncl_ */
data coude_acm;
    set tree_acm;
    by _ncl_;
    if first._ncl_;
    keep _ncl_ _sprsq_ _rsq_;
run;

/* Graphique du coude : on cherche le saut le plus important de SPRSQ
   Un grand saut indique qu'on fusionne deux groupes très distincts
   → on s'arrête juste avant ce saut */
proc sgplot data=coude_acm;
    where _ncl_ > 0 and _ncl_ <= 20;
    series x=_ncl_ y=_sprsq_ / markers lineattrs=(thickness=2);
    xaxis reverse label="Nombre de classes";
    yaxis label="R² semi-partiel (_SPRSQ_)";
    title "Graphique du coude - Classification hierarchique";
run;

/* ── 11. ÉVOLUTION DU R² GLOBAL ─────────────────────────────────────────── */

/* On cherche le coude où le R² se stabilise :
   au-delà de ce point, ajouter une classe n'apporte plus d'information */
proc sgplot data=coude_acm;
    where _ncl_ > 0 and _ncl_ <= 20;
    series x=_ncl_ y=_rsq_ / markers lineattrs=(thickness=2);
    xaxis reverse label="Nombre de classes";
    yaxis label="R² global (_RSQ_)";
    title "Evolution du R² global selon le nombre de classes";
run;

/* ── 12. DÉCOUPAGE EN 3 CLASSES ─────────────────────────────────────────── */

/* On coupe l'arbre en 3 classes selon les graphiques du coude
   out=classes_acm : dataset avec une variable CLUSTER par modalité */
proc tree data=tree_acm nclusters=3 out=classes_acm;
    id _name_;
run;

/* Vérification du nom exact de la variable de classe générée
   SAS peut nommer la variable CLUSTER ou _CLUSTER_ selon la version */
proc contents data=classes_acm;
    title "Verification du nom de la variable de groupe creee par la PROC TREE";
run;

/* Aperçu de la répartition des modalités dans les 3 classes */
proc print data=classes_acm (obs=20);
    title "Apercu de la repartition des modalites dans les 3 classes";
run;

/* Affichage complet de toutes les modalités avec leur classe */
proc print data=classes_acm;
    title "Repartition des modalites dans les 3 classes";
run;

/* ── 13. PROJECTION ET COLORATION DES CLUSTERS SUR LE PLAN FACTORIEL ────── */

/* Tri par _name_ dans les deux tables avant la fusion */
proc sort data=coord_var;   by _name_; run;
proc sort data=classes_acm; by _name_; run;

/* Fusion des coordonnées ACM avec les numéros de classe
   in=a et in=b : on ne garde que les observations présentes dans les deux tables */
data plot_classes_acm;
    merge coord_var (in=a) classes_acm (in=b);
    by _name_;
    if a and b;
run;

/* Graphique final : plan factoriel coloré par cluster de modalités
   group=cluster : une couleur par classe
   datalabel=_name_ : nom de la modalité affiché sur le graphique
   Ce graphique permet d'interpréter chaque cluster de modalités
   et de nommer les profils-types de voitures identifiés */
proc sgplot data=plot_classes_acm;
    scatter x=dim1 y=dim2 / group=cluster datalabel=_name_;
    yaxis label="Axe 2";
    title "Modalites projetees sur le plan factoriel et colorees par classe";
run;