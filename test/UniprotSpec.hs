{-# LANGUAGE QuasiQuotes       #-}
module UniprotSpec where

import           Prelude       hiding (lines, unlines)

import           Data.Text            (Text)
import           Data.Attoparsec.Text (parseOnly, endOfLine, many')
import           NeatInterpolation    (text)
import           Test.Hspec

import          Bio.Uniprot.Type
import          Bio.Uniprot.Parser

idStr :: Text
idStr = "ID   PDCD1_HUMAN             Reviewed;         288 AA."

acStr :: Text
acStr = "AC   Q15116; O00517; Q8IX89;"

dtStr :: Text
dtStr = [text|
DT   01-NOV-1997, integrated into UniProtKB/Swiss-Prot.
DT   17-APR-2007, sequence version 3.
DT   28-FEB-2018, entry version 163.|]

deStr :: Text
deStr = [text|
DE   RecName: Full=Programmed cell death protein 1;
DE            Short=Protein PD-1;
DE            Short=hPD-1;
DE   AltName: CD_antigen=CD279;
DE   Flags: Precursor;|]

de2Str :: Text
de2Str = [text|
DE   RecName: Full=Arginine biosynthesis bifunctional protein argJ;
DE   Includes:
DE     RecName: Full=Glutamate N-acetyltransferase;
DE              EC=2.3.1.35;
DE     AltName: Full=Ornithine acetyltransferase;
DE              Short=OATase;
DE     AltName: Full=Ornithine transacetylase;
DE   Includes:
DE     RecName: Full=Amino-acid acetyltransferase;
DE              EC=2.3.1.1;
DE     AltName: Full=N-acetylglutamate synthase;
DE              Short=AGS;
DE   Contains:
DE     RecName: Full=Arginine biosynthesis bifunctional protein argJ alpha chain;
DE   Contains:
DE     RecName: Full=Arginine biosynthesis bifunctional protein argJ beta chain;|]
  
gnStr :: Text
gnStr = "GN   Name=PDCD1; Synonyms=PD1;"

osStr :: Text
osStr = "OS   Homo sapiens (Human)."

ogStr :: Text
ogStr = [text|
OG   Plasmid R6-5, Plasmid IncFII R100 (NR1), and
OG   Plasmid IncFII R1-19 (R1 drd-19).|]

ocStr :: Text
ocStr = [text|
OC   Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi;
OC   Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini;
OC   Catarrhini; Hominidae; Homo.|]

oxStr :: Text
oxStr = "OX   NCBI_TaxID=9606;"

ohStr :: Text
ohStr = "OH   NCBI_TaxID=9536; Cercopithecus hamlyni (Owl-faced monkey) (Hamlyn's monkey)."

refStr :: Text
refStr = [text|
RN   [1]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA].
RX   PubMed=7851902; DOI=10.1006/geno.1994.1562;
RA   Shinohara T., Taniwaki M., Ishida Y., Kawaich M., Honjo T.;
RT   "Structure and chromosomal localization of the human PD-1 gene
RT   (PDCD1).";
RL   Genomics 23:704-706(1994).
RN   [2]
RP   NUCLEOTIDE SEQUENCE [MRNA].
RX   PubMed=9332365; DOI=10.1016/S0378-1119(97)00260-6;
RA   Finger L.R., Pu J., Wasserman R., Vibhakar R., Louie E., Hardy R.R.,
RA   Burrows P.D., Billips L.D.;
RT   "The human PD-1 gene: complete cDNA, genomic organization, and
RT   developmentally regulated expression in B cell progenitors.";
RL   Gene 197:177-187(1997).
RN   [3]
RP   ERRATUM.
RA   Finger L.R., Pu J., Wasserman R., Vibhakar R., Louie E., Hardy R.R.,
RA   Burrows P.D., Billips L.D.;
RL   Gene 203:253-253(1997).
RN   [4]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA], AND INVOLVEMENT IN SLEB2.
RX   PubMed=12402038; DOI=10.1038/ng1020;
RA   Prokunina L., Castillejo-Lopez C., Oberg F., Gunnarsson I., Berg L.,
RA   Magnusson V., Brookes A.J., Tentler D., Kristjansdottir H.,
RA   Grondal G., Bolstad A.I., Svenungsson E., Lundberg I., Sturfelt G.,
RA   Jonssen A., Truedsson L., Lima G., Alcocer-Varela J., Jonsson R.,
RA   Gyllensten U.B., Harley J.B., Alarcon-Segovia D., Steinsson K.,
RA   Alarcon-Riquelme M.E.;
RT   "A regulatory polymorphism in PDCD1 is associated with susceptibility
RT   to systemic lupus erythematosus in humans.";
RL   Nat. Genet. 32:666-669(2002).
RN   [5]
RP   NUCLEOTIDE SEQUENCE [MRNA].
RA   He X., Xu L., Liu Y., Zeng Y.;
RT   "Cloning of PD-1 cDNA from activated peripheral leukocytes.";
RL   Submitted (FEB-2003) to the EMBL/GenBank/DDBJ databases.
RN   [6]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA].
RA   Livingston R.J., Shaffer T., McFarland I., Nguyen C.P., Stanaway I.B.,
RA   Rajkumar N., Johnson E.J., da Ponte S.H., Willa H., Ahearn M.O.,
RA   Bertucci C., Acklestad J., Carroll A., Swanson J., Gildersleeve H.I.,
RA   Nickerson D.A.;
RL   Submitted (OCT-2006) to the EMBL/GenBank/DDBJ databases.
RN   [7]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE MRNA].
RX   PubMed=14702039; DOI=10.1038/ng1285;
RA   Ota T., Suzuki Y., Nishikawa T., Otsuki T., Sugiyama T., Irie R.,
RA   Wakamatsu A., Hayashi K., Sato H., Nagai K., Kimura K., Makita H.,
RA   Sekine M., Obayashi M., Nishi T., Shibahara T., Tanaka T., Ishii S.,
RA   Yamamoto J., Saito K., Kawai Y., Isono Y., Nakamura Y., Nagahari K.,
RA   Murakami K., Yasuda T., Iwayanagi T., Wagatsuma M., Shiratori A.,
RA   Sudo H., Hosoiri T., Kaku Y., Kodaira H., Kondo H., Sugawara M.,
RA   Takahashi M., Kanda K., Yokoi T., Furuya T., Kikkawa E., Omura Y.,
RA   Abe K., Kamihara K., Katsuta N., Sato K., Tanikawa M., Yamazaki M.,
RA   Ninomiya K., Ishibashi T., Yamashita H., Murakawa K., Fujimori K.,
RA   Tanai H., Kimata M., Watanabe M., Hiraoka S., Chiba Y., Ishida S.,
RA   Ono Y., Takiguchi S., Watanabe S., Yosida M., Hotuta T., Kusano J.,
RA   Kanehori K., Takahashi-Fujii A., Hara H., Tanase T.-O., Nomura Y.,
RA   Togiya S., Komai F., Hara R., Takeuchi K., Arita M., Imose N.,
RA   Musashino K., Yuuki H., Oshima A., Sasaki N., Aotsuka S.,
RA   Yoshikawa Y., Matsunawa H., Ichihara T., Shiohata N., Sano S.,
RA   Moriya S., Momiyama H., Satoh N., Takami S., Terashima Y., Suzuki O.,
RA   Nakagawa S., Senoh A., Mizoguchi H., Goto Y., Shimizu F., Wakebe H.,
RA   Hishigaki H., Watanabe T., Sugiyama A., Takemoto M., Kawakami B.,
RA   Yamazaki M., Watanabe K., Kumagai A., Itakura S., Fukuzumi Y.,
RA   Fujimori Y., Komiyama M., Tashiro H., Tanigami A., Fujiwara T.,
RA   Ono T., Yamada K., Fujii Y., Ozaki K., Hirao M., Ohmori Y.,
RA   Kawabata A., Hikiji T., Kobatake N., Inagaki H., Ikema Y., Okamoto S.,
RA   Okitani R., Kawakami T., Noguchi S., Itoh T., Shigeta K., Senba T.,
RA   Matsumura K., Nakajima Y., Mizuno T., Morinaga M., Sasaki M.,
RA   Togashi T., Oyama M., Hata H., Watanabe M., Komatsu T.,
RA   Mizushima-Sugano J., Satoh T., Shirai Y., Takahashi Y., Nakagawa K.,
RA   Okumura K., Nagase T., Nomura N., Kikuchi H., Masuho Y., Yamashita R.,
RA   Nakai K., Yada T., Nakamura Y., Ohara O., Isogai T., Sugano S.;
RT   "Complete sequencing and characterization of 21,243 full-length human
RT   cDNAs.";
RL   Nat. Genet. 36:40-45(2004).
RN   [8]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE MRNA].
RX   PubMed=15489334; DOI=10.1101/gr.2596504;
RG   The MGC Project Team;
RT   "The status, quality, and expansion of the NIH full-length cDNA
RT   project: the Mammalian Gene Collection (MGC).";
RL   Genome Res. 14:2121-2127(2004).
RN   [9]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE GENOMIC DNA].
RA   Mural R.J., Istrail S., Sutton G., Florea L., Halpern A.L.,
RA   Mobarry C.M., Lippert R., Walenz B., Shatkay H., Dew I., Miller J.R.,
RA   Flanigan M.J., Edwards N.J., Bolanos R., Fasulo D., Halldorsson B.V.,
RA   Hannenhalli S., Turner R., Yooseph S., Lu F., Nusskern D.R.,
RA   Shue B.C., Zheng X.H., Zhong F., Delcher A.L., Huson D.H.,
RA   Kravitz S.A., Mouchard L., Reinert K., Remington K.A., Clark A.G.,
RA   Waterman M.S., Eichler E.E., Adams M.D., Hunkapiller M.W., Myers E.W.,
RA   Venter J.C.;
RL   Submitted (JUL-2005) to the EMBL/GenBank/DDBJ databases.
RN   [10]
RP   FUNCTION.
RX   PubMed=21276005; DOI=10.1111/j.1749-6632.2010.05919.x;
RA   Fife B.T., Pauken K.E.;
RT   "The role of the PD-1 pathway in autoimmunity and peripheral
RT   tolerance.";
RL   Ann. N. Y. Acad. Sci. 1217:45-59(2011).|]

ccStr :: Text
ccStr = [text|
CC   -!- FUNCTION: Inhibitory cell surface receptor involved in the
CC       regulation of T-cell function during immunity and tolerance. Upon
CC       ligand binding, inhibits T-cell effector functions in an antigen-
CC       specific manner. Possible cell death inducer, in association with
CC       other factors. {ECO:0000269|PubMed:21276005}.
CC   -!- SUBUNIT: Monomer. {ECO:0000250}.
CC   -!- INTERACTION:
CC       Q9NZQ7:CD274; NbExp=2; IntAct=EBI-4314328, EBI-4314282;
CC       Q9NZQ7-1:CD274; NbExp=2; IntAct=EBI-4314328, EBI-15686469;
CC       Q06124:PTPN11; NbExp=3; IntAct=EBI-4314328, EBI-297779;
CC   -!- SUBCELLULAR LOCATION: Membrane; Single-pass type I membrane
CC       protein.
CC   -!- DEVELOPMENTAL STAGE: Induced at programmed cell death.
CC   -!- DISEASE: Systemic lupus erythematosus 2 (SLEB2) [MIM:605218]: A
CC       chronic, relapsing, inflammatory, and often febrile multisystemic
CC       disorder of connective tissue, characterized principally by
CC       involvement of the skin, joints, kidneys and serosal membranes. It
CC       is of unknown etiology, but is thought to represent a failure of
CC       the regulatory mechanisms of the autoimmune system. The disease is
CC       marked by a wide range of system dysfunctions, an elevated
CC       erythrocyte sedimentation rate, and the formation of LE cells in
CC       the blood or bone marrow. {ECO:0000269|PubMed:12402038}.
CC       Note=Disease susceptibility is associated with variations
CC       affecting the gene represented in this entry.|]

peStr :: Text
peStr = "PE   1: Evidence at protein level;"

kwStr :: Text
kwStr = [text|
KW   3D-structure; Apoptosis; Complete proteome; Disulfide bond;
KW   Glycoprotein; Immunity; Immunoglobulin domain; Membrane; Polymorphism;
KW   Reference proteome; Signal; Systemic lupus erythematosus;
KW   Transmembrane; Transmembrane helix.|]

drStr :: Text
drStr = [text|
DR   EMBL; L27440; AAC41700.1; -; Genomic_DNA.
DR   EMBL; U64863; AAC51773.1; -; mRNA.
DR   EMBL; AF363458; AAN64003.1; -; Genomic_DNA.
DR   EMBL; AY238517; AAO63583.1; -; mRNA.
DR   EMBL; EF064716; ABK41899.1; -; Genomic_DNA.
DR   EMBL; AK313848; BAG36577.1; -; mRNA.
DR   EMBL; CH471063; EAW71298.1; -; Genomic_DNA.
DR   EMBL; BC074740; AAH74740.1; -; mRNA.
DR   CCDS; CCDS33428.1; -.
DR   PIR; A55737; A55737.
DR   RefSeq; NP_005009.2; NM_005018.2.
DR   UniGene; Hs.158297; -.
DR   PDB; 2M2D; NMR; -; A=34-150.
DR   PDB; 3RRQ; X-ray; 2.10 A; A=32-160.
DR   PDB; 4ZQK; X-ray; 2.45 A; B=33-150.
DR   PDB; 5B8C; X-ray; 2.15 A; C/F/I/L=32-160.
DR   PDB; 5GGR; X-ray; 3.30 A; Y/Z=26-150.
DR   PDB; 5GGS; X-ray; 2.00 A; Y/Z=26-148.
DR   PDB; 5IUS; X-ray; 2.89 A; A/B=26-146.
DR   PDB; 5JXE; X-ray; 2.90 A; A/B=33-146.
DR   PDB; 5WT9; X-ray; 2.40 A; G=1-167.
DR   PDBsum; 2M2D; -.
DR   PDBsum; 3RRQ; -.
DR   PDBsum; 4ZQK; -.
DR   PDBsum; 5B8C; -.
DR   PDBsum; 5GGR; -.
DR   PDBsum; 5GGS; -.
DR   PDBsum; 5IUS; -.
DR   PDBsum; 5JXE; -.
DR   PDBsum; 5WT9; -.
DR   ProteinModelPortal; Q15116; -.
DR   SMR; Q15116; -.
DR   BioGrid; 111160; 61.
DR   DIP; DIP-44126N; -.
DR   IntAct; Q15116; 5.
DR   MINT; Q15116; -.
DR   STRING; 9606.ENSP00000335062; -.
DR   ChEMBL; CHEMBL3307223; -.
DR   DrugBank; DB05916; CT-011.
DR   DrugBank; DB09035; Nivolumab.
DR   DrugBank; DB09037; Pembrolizumab.
DR   GuidetoPHARMACOLOGY; 2760; -.
DR   iPTMnet; Q15116; -.
DR   PhosphoSitePlus; Q15116; -.
DR   BioMuta; PDCD1; -.
DR   DMDM; 145559515; -.
DR   PaxDb; Q15116; -.
DR   PeptideAtlas; Q15116; -.
DR   PRIDE; Q15116; -.
DR   DNASU; 5133; -.
DR   Ensembl; ENST00000334409; ENSP00000335062; ENSG00000188389.
DR   Ensembl; ENST00000618185; ENSP00000480684; ENSG00000276977.
DR   GeneID; 5133; -.
DR   KEGG; hsa:5133; -.
DR   UCSC; uc002wcq.5; human.
DR   CTD; 5133; -.
DR   DisGeNET; 5133; -.
DR   EuPathDB; HostDB:ENSG00000188389.10; -.
DR   GeneCards; PDCD1; -.
DR   H-InvDB; HIX0030684; -.
DR   HGNC; HGNC:8760; PDCD1.
DR   HPA; CAB038418; -.
DR   HPA; HPA035981; -.
DR   MalaCards; PDCD1; -.
DR   MIM; 109100; phenotype.
DR   MIM; 600244; gene.
DR   MIM; 605218; phenotype.
DR   neXtProt; NX_Q15116; -.
DR   OpenTargets; ENSG00000188389; -.
DR   Orphanet; 802; Multiple sclerosis.
DR   Orphanet; 536; Systemic lupus erythematosus.
DR   PharmGKB; PA33110; -.
DR   eggNOG; ENOG410J26W; Eukaryota.
DR   eggNOG; ENOG41116U6; LUCA.
DR   GeneTree; ENSGT00390000013662; -.
DR   HOGENOM; HOG000253959; -.
DR   HOVERGEN; HBG053534; -.
DR   InParanoid; Q15116; -.
DR   KO; K06744; -.
DR   OMA; DFQWREK; -.
DR   OrthoDB; EOG091G0EE8; -.
DR   PhylomeDB; Q15116; -.
DR   TreeFam; TF336181; -.
DR   Reactome; R-HSA-389948; PD-1 signaling.
DR   SIGNOR; Q15116; -.
DR   ChiTaRS; PDCD1; human.
DR   GeneWiki; Programmed_cell_death_1; -.
DR   GenomeRNAi; 5133; -.
DR   PRO; PR:Q15116; -.
DR   Proteomes; UP000005640; Chromosome 2.
DR   Bgee; ENSG00000188389; -.
DR   CleanEx; HS_PDCD1; -.
DR   ExpressionAtlas; Q15116; baseline and differential.
DR   Genevisible; Q15116; HS.
DR   GO; GO:0009897; C:external side of plasma membrane; IEA:Ensembl.
DR   GO; GO:0016021; C:integral component of membrane; IEA:UniProtKB-KW.
DR   GO; GO:0005886; C:plasma membrane; TAS:Reactome.
DR   GO; GO:0004871; F:signal transducer activity; TAS:ProtInc.
DR   GO; GO:0006915; P:apoptotic process; TAS:ProtInc.
DR   GO; GO:0006959; P:humoral immune response; TAS:ProtInc.
DR   GO; GO:0007275; P:multicellular organism development; TAS:ProtInc.
DR   GO; GO:0043066; P:negative regulation of apoptotic process; IEA:Ensembl.
DR   GO; GO:0002644; P:negative regulation of tolerance induction; IEA:Ensembl.
DR   GO; GO:0070234; P:positive regulation of T cell apoptotic process; IDA:UniProtKB.
DR   GO; GO:0031295; P:T cell costimulation; TAS:Reactome.
DR   Gene3D; 2.60.40.10; -; 1.
DR   InterPro; IPR007110; Ig-like_dom.
DR   InterPro; IPR036179; Ig-like_dom_sf.
DR   InterPro; IPR013783; Ig-like_fold.
DR   InterPro; IPR003599; Ig_sub.
DR   InterPro; IPR013106; Ig_V-set.
DR   Pfam; PF07686; V-set; 1.
DR   SMART; SM00409; IG; 1.
DR   SMART; SM00406; IGv; 1.
DR   SUPFAM; SSF48726; SSF48726; 1.
DR   PROSITE; PS50835; IG_LIKE; 1.|]

ftStr :: Text
ftStr = [text|
FT   SIGNAL        1     20       {ECO:0000255}.
FT   CHAIN        21    288       Programmed cell death protein 1.
FT                                /FTId=PRO_0000014892.
FT   TOPO_DOM     21    170       Extracellular. {ECO:0000255}.
FT   TRANSMEM    171    191       Helical. {ECO:0000255}.
FT   TOPO_DOM    192    288       Cytoplasmic. {ECO:0000255}.
FT   DOMAIN       35    145       Ig-like V-type.
FT   CARBOHYD     49     49       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD     58     58       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD     74     74       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD    116    116       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   DISULFID     54    123       {ECO:0000255|PROSITE-ProRule:PRU00114}.
FT   VARIANT     215    215       A -> V (in dbSNP:rs2227982).
FT                                /FTId=VAR_031685.
FT   CONFLICT     38     38       S -> F (in Ref. 2; AAC51773).
FT                                {ECO:0000305}.
FT   CONFLICT    162    162       P -> S (in Ref. 1; AAC41700).
FT                                {ECO:0000305}.
FT   STRAND       27     29       {ECO:0000244|PDB:5WT9}.
FT   STRAND       36     38       {ECO:0000244|PDB:5GGS}.
FT   STRAND       40     45       {ECO:0000244|PDB:5GGS}.
FT   STRAND       50     55       {ECO:0000244|PDB:5GGS}.
FT   STRAND       60     70       {ECO:0000244|PDB:5GGS}.
FT   STRAND       72     74       {ECO:0000244|PDB:4ZQK}.
FT   STRAND       76     83       {ECO:0000244|PDB:5GGS}.
FT   STRAND       95     99       {ECO:0000244|PDB:5GGS}.
FT   STRAND      103    112       {ECO:0000244|PDB:5GGS}.
FT   HELIX       115    117       {ECO:0000244|PDB:5GGS}.
FT   STRAND      119    131       {ECO:0000244|PDB:5GGS}.
FT   STRAND      134    136       {ECO:0000244|PDB:5GGS}.
FT   STRAND      140    145       {ECO:0000244|PDB:5GGS}.|]
  
sqStr :: Text
sqStr = [text|
SQ   SEQUENCE   288 AA;  31647 MW;  A5210FD40C304FB7 CRC64;
     MQIPQAPWPV VWAVLQLGWR PGWFLDSPDR PWNPPTFSPA LLVVTEGDNA TFTCSFSNTS
     ESFVLNWYRM SPSNQTDKLA AFPEDRSQPG QDCRFRVTQL PNGRDFHMSV VRARRNDSGT
     YLCGAISLAP KAQIKESLRA ELRVTERRAE VPTAHPSPSP RPAGQFQTLV VGVVGGLLGS
     LVLLVWVLAV ICSRAARGTI GARRTGQPLK EDPSAVPVFS VDYGELDFQW REKTPEPPVP
     CVPEQTEYAT IVFPSGMGTS SPARRGSADG PRSAQPLRPE DGHCSWPL|]

endStr :: Text
endStr = "//"

pd1Str :: Text
pd1Str = [text|
ID   PDCD1_HUMAN             Reviewed;         288 AA.
AC   Q15116; O00517; Q8IX89;
DT   01-NOV-1997, integrated into UniProtKB/Swiss-Prot.
DT   17-APR-2007, sequence version 3.
DT   28-FEB-2018, entry version 163.
DE   RecName: Full=Programmed cell death protein 1;
DE            Short=Protein PD-1;
DE            Short=hPD-1;
DE   AltName: CD_antigen=CD279;
DE   Flags: Precursor;
GN   Name=PDCD1; Synonyms=PD1;
OS   Homo sapiens (Human).
OC   Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi;
OC   Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini;
OC   Catarrhini; Hominidae; Homo.
OX   NCBI_TaxID=9606;
RN   [1]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA].
RX   PubMed=7851902; DOI=10.1006/geno.1994.1562;
RA   Shinohara T., Taniwaki M., Ishida Y., Kawaich M., Honjo T.;
RT   "Structure and chromosomal localization of the human PD-1 gene
RT   (PDCD1).";
RL   Genomics 23:704-706(1994).
RN   [2]
RP   NUCLEOTIDE SEQUENCE [MRNA].
RX   PubMed=9332365; DOI=10.1016/S0378-1119(97)00260-6;
RA   Finger L.R., Pu J., Wasserman R., Vibhakar R., Louie E., Hardy R.R.,
RA   Burrows P.D., Billips L.D.;
RT   "The human PD-1 gene: complete cDNA, genomic organization, and
RT   developmentally regulated expression in B cell progenitors.";
RL   Gene 197:177-187(1997).
RN   [3]
RP   ERRATUM.
RA   Finger L.R., Pu J., Wasserman R., Vibhakar R., Louie E., Hardy R.R.,
RA   Burrows P.D., Billips L.D.;
RL   Gene 203:253-253(1997).
RN   [4]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA], AND INVOLVEMENT IN SLEB2.
RX   PubMed=12402038; DOI=10.1038/ng1020;
RA   Prokunina L., Castillejo-Lopez C., Oberg F., Gunnarsson I., Berg L.,
RA   Magnusson V., Brookes A.J., Tentler D., Kristjansdottir H.,
RA   Grondal G., Bolstad A.I., Svenungsson E., Lundberg I., Sturfelt G.,
RA   Jonssen A., Truedsson L., Lima G., Alcocer-Varela J., Jonsson R.,
RA   Gyllensten U.B., Harley J.B., Alarcon-Segovia D., Steinsson K.,
RA   Alarcon-Riquelme M.E.;
RT   "A regulatory polymorphism in PDCD1 is associated with susceptibility
RT   to systemic lupus erythematosus in humans.";
RL   Nat. Genet. 32:666-669(2002).
RN   [5]
RP   NUCLEOTIDE SEQUENCE [MRNA].
RA   He X., Xu L., Liu Y., Zeng Y.;
RT   "Cloning of PD-1 cDNA from activated peripheral leukocytes.";
RL   Submitted (FEB-2003) to the EMBL/GenBank/DDBJ databases.
RN   [6]
RP   NUCLEOTIDE SEQUENCE [GENOMIC DNA].
RA   Livingston R.J., Shaffer T., McFarland I., Nguyen C.P., Stanaway I.B.,
RA   Rajkumar N., Johnson E.J., da Ponte S.H., Willa H., Ahearn M.O.,
RA   Bertucci C., Acklestad J., Carroll A., Swanson J., Gildersleeve H.I.,
RA   Nickerson D.A.;
RL   Submitted (OCT-2006) to the EMBL/GenBank/DDBJ databases.
RN   [7]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE MRNA].
RX   PubMed=14702039; DOI=10.1038/ng1285;
RA   Ota T., Suzuki Y., Nishikawa T., Otsuki T., Sugiyama T., Irie R.,
RA   Wakamatsu A., Hayashi K., Sato H., Nagai K., Kimura K., Makita H.,
RA   Sekine M., Obayashi M., Nishi T., Shibahara T., Tanaka T., Ishii S.,
RA   Yamamoto J., Saito K., Kawai Y., Isono Y., Nakamura Y., Nagahari K.,
RA   Murakami K., Yasuda T., Iwayanagi T., Wagatsuma M., Shiratori A.,
RA   Sudo H., Hosoiri T., Kaku Y., Kodaira H., Kondo H., Sugawara M.,
RA   Takahashi M., Kanda K., Yokoi T., Furuya T., Kikkawa E., Omura Y.,
RA   Abe K., Kamihara K., Katsuta N., Sato K., Tanikawa M., Yamazaki M.,
RA   Ninomiya K., Ishibashi T., Yamashita H., Murakawa K., Fujimori K.,
RA   Tanai H., Kimata M., Watanabe M., Hiraoka S., Chiba Y., Ishida S.,
RA   Ono Y., Takiguchi S., Watanabe S., Yosida M., Hotuta T., Kusano J.,
RA   Kanehori K., Takahashi-Fujii A., Hara H., Tanase T.-O., Nomura Y.,
RA   Togiya S., Komai F., Hara R., Takeuchi K., Arita M., Imose N.,
RA   Musashino K., Yuuki H., Oshima A., Sasaki N., Aotsuka S.,
RA   Yoshikawa Y., Matsunawa H., Ichihara T., Shiohata N., Sano S.,
RA   Moriya S., Momiyama H., Satoh N., Takami S., Terashima Y., Suzuki O.,
RA   Nakagawa S., Senoh A., Mizoguchi H., Goto Y., Shimizu F., Wakebe H.,
RA   Hishigaki H., Watanabe T., Sugiyama A., Takemoto M., Kawakami B.,
RA   Yamazaki M., Watanabe K., Kumagai A., Itakura S., Fukuzumi Y.,
RA   Fujimori Y., Komiyama M., Tashiro H., Tanigami A., Fujiwara T.,
RA   Ono T., Yamada K., Fujii Y., Ozaki K., Hirao M., Ohmori Y.,
RA   Kawabata A., Hikiji T., Kobatake N., Inagaki H., Ikema Y., Okamoto S.,
RA   Okitani R., Kawakami T., Noguchi S., Itoh T., Shigeta K., Senba T.,
RA   Matsumura K., Nakajima Y., Mizuno T., Morinaga M., Sasaki M.,
RA   Togashi T., Oyama M., Hata H., Watanabe M., Komatsu T.,
RA   Mizushima-Sugano J., Satoh T., Shirai Y., Takahashi Y., Nakagawa K.,
RA   Okumura K., Nagase T., Nomura N., Kikuchi H., Masuho Y., Yamashita R.,
RA   Nakai K., Yada T., Nakamura Y., Ohara O., Isogai T., Sugano S.;
RT   "Complete sequencing and characterization of 21,243 full-length human
RT   cDNAs.";
RL   Nat. Genet. 36:40-45(2004).
RN   [8]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE MRNA].
RX   PubMed=15489334; DOI=10.1101/gr.2596504;
RG   The MGC Project Team;
RT   "The status, quality, and expansion of the NIH full-length cDNA
RT   project: the Mammalian Gene Collection (MGC).";
RL   Genome Res. 14:2121-2127(2004).
RN   [9]
RP   NUCLEOTIDE SEQUENCE [LARGE SCALE GENOMIC DNA].
RA   Mural R.J., Istrail S., Sutton G., Florea L., Halpern A.L.,
RA   Mobarry C.M., Lippert R., Walenz B., Shatkay H., Dew I., Miller J.R.,
RA   Flanigan M.J., Edwards N.J., Bolanos R., Fasulo D., Halldorsson B.V.,
RA   Hannenhalli S., Turner R., Yooseph S., Lu F., Nusskern D.R.,
RA   Shue B.C., Zheng X.H., Zhong F., Delcher A.L., Huson D.H.,
RA   Kravitz S.A., Mouchard L., Reinert K., Remington K.A., Clark A.G.,
RA   Waterman M.S., Eichler E.E., Adams M.D., Hunkapiller M.W., Myers E.W.,
RA   Venter J.C.;
RL   Submitted (JUL-2005) to the EMBL/GenBank/DDBJ databases.
RN   [10]
RP   FUNCTION.
RX   PubMed=21276005; DOI=10.1111/j.1749-6632.2010.05919.x;
RA   Fife B.T., Pauken K.E.;
RT   "The role of the PD-1 pathway in autoimmunity and peripheral
RT   tolerance.";
RL   Ann. N. Y. Acad. Sci. 1217:45-59(2011).
CC   -!- FUNCTION: Inhibitory cell surface receptor involved in the
CC       regulation of T-cell function during immunity and tolerance. Upon
CC       ligand binding, inhibits T-cell effector functions in an antigen-
CC       specific manner. Possible cell death inducer, in association with
CC       other factors. {ECO:0000269|PubMed:21276005}.
CC   -!- SUBUNIT: Monomer. {ECO:0000250}.
CC   -!- INTERACTION:
CC       Q9NZQ7:CD274; NbExp=2; IntAct=EBI-4314328, EBI-4314282;
CC       Q9NZQ7-1:CD274; NbExp=2; IntAct=EBI-4314328, EBI-15686469;
CC       Q06124:PTPN11; NbExp=3; IntAct=EBI-4314328, EBI-297779;
CC   -!- SUBCELLULAR LOCATION: Membrane; Single-pass type I membrane
CC       protein.
CC   -!- DEVELOPMENTAL STAGE: Induced at programmed cell death.
CC   -!- DISEASE: Systemic lupus erythematosus 2 (SLEB2) [MIM:605218]: A
CC       chronic, relapsing, inflammatory, and often febrile multisystemic
CC       disorder of connective tissue, characterized principally by
CC       involvement of the skin, joints, kidneys and serosal membranes. It
CC       is of unknown etiology, but is thought to represent a failure of
CC       the regulatory mechanisms of the autoimmune system. The disease is
CC       marked by a wide range of system dysfunctions, an elevated
CC       erythrocyte sedimentation rate, and the formation of LE cells in
CC       the blood or bone marrow. {ECO:0000269|PubMed:12402038}.
CC       Note=Disease susceptibility is associated with variations
CC       affecting the gene represented in this entry.
DR   EMBL; L27440; AAC41700.1; -; Genomic_DNA.
DR   EMBL; U64863; AAC51773.1; -; mRNA.
DR   EMBL; AF363458; AAN64003.1; -; Genomic_DNA.
DR   EMBL; AY238517; AAO63583.1; -; mRNA.
DR   EMBL; EF064716; ABK41899.1; -; Genomic_DNA.
DR   EMBL; AK313848; BAG36577.1; -; mRNA.
DR   EMBL; CH471063; EAW71298.1; -; Genomic_DNA.
DR   EMBL; BC074740; AAH74740.1; -; mRNA.
DR   CCDS; CCDS33428.1; -.
DR   PIR; A55737; A55737.
DR   RefSeq; NP_005009.2; NM_005018.2.
DR   UniGene; Hs.158297; -.
DR   PDB; 2M2D; NMR; -; A=34-150.
DR   PDB; 3RRQ; X-ray; 2.10 A; A=32-160.
DR   PDB; 4ZQK; X-ray; 2.45 A; B=33-150.
DR   PDB; 5B8C; X-ray; 2.15 A; C/F/I/L=32-160.
DR   PDB; 5GGR; X-ray; 3.30 A; Y/Z=26-150.
DR   PDB; 5GGS; X-ray; 2.00 A; Y/Z=26-148.
DR   PDB; 5IUS; X-ray; 2.89 A; A/B=26-146.
DR   PDB; 5JXE; X-ray; 2.90 A; A/B=33-146.
DR   PDB; 5WT9; X-ray; 2.40 A; G=1-167.
DR   PDBsum; 2M2D; -.
DR   PDBsum; 3RRQ; -.
DR   PDBsum; 4ZQK; -.
DR   PDBsum; 5B8C; -.
DR   PDBsum; 5GGR; -.
DR   PDBsum; 5GGS; -.
DR   PDBsum; 5IUS; -.
DR   PDBsum; 5JXE; -.
DR   PDBsum; 5WT9; -.
DR   ProteinModelPortal; Q15116; -.
DR   SMR; Q15116; -.
DR   BioGrid; 111160; 61.
DR   DIP; DIP-44126N; -.
DR   IntAct; Q15116; 5.
DR   MINT; Q15116; -.
DR   STRING; 9606.ENSP00000335062; -.
DR   ChEMBL; CHEMBL3307223; -.
DR   DrugBank; DB05916; CT-011.
DR   DrugBank; DB09035; Nivolumab.
DR   DrugBank; DB09037; Pembrolizumab.
DR   GuidetoPHARMACOLOGY; 2760; -.
DR   iPTMnet; Q15116; -.
DR   PhosphoSitePlus; Q15116; -.
DR   BioMuta; PDCD1; -.
DR   DMDM; 145559515; -.
DR   PaxDb; Q15116; -.
DR   PeptideAtlas; Q15116; -.
DR   PRIDE; Q15116; -.
DR   DNASU; 5133; -.
DR   Ensembl; ENST00000334409; ENSP00000335062; ENSG00000188389.
DR   Ensembl; ENST00000618185; ENSP00000480684; ENSG00000276977.
DR   GeneID; 5133; -.
DR   KEGG; hsa:5133; -.
DR   UCSC; uc002wcq.5; human.
DR   CTD; 5133; -.
DR   DisGeNET; 5133; -.
DR   EuPathDB; HostDB:ENSG00000188389.10; -.
DR   GeneCards; PDCD1; -.
DR   H-InvDB; HIX0030684; -.
DR   HGNC; HGNC:8760; PDCD1.
DR   HPA; CAB038418; -.
DR   HPA; HPA035981; -.
DR   MalaCards; PDCD1; -.
DR   MIM; 109100; phenotype.
DR   MIM; 600244; gene.
DR   MIM; 605218; phenotype.
DR   neXtProt; NX_Q15116; -.
DR   OpenTargets; ENSG00000188389; -.
DR   Orphanet; 802; Multiple sclerosis.
DR   Orphanet; 536; Systemic lupus erythematosus.
DR   PharmGKB; PA33110; -.
DR   eggNOG; ENOG410J26W; Eukaryota.
DR   eggNOG; ENOG41116U6; LUCA.
DR   GeneTree; ENSGT00390000013662; -.
DR   HOGENOM; HOG000253959; -.
DR   HOVERGEN; HBG053534; -.
DR   InParanoid; Q15116; -.
DR   KO; K06744; -.
DR   OMA; DFQWREK; -.
DR   OrthoDB; EOG091G0EE8; -.
DR   PhylomeDB; Q15116; -.
DR   TreeFam; TF336181; -.
DR   Reactome; R-HSA-389948; PD-1 signaling.
DR   SIGNOR; Q15116; -.
DR   ChiTaRS; PDCD1; human.
DR   GeneWiki; Programmed_cell_death_1; -.
DR   GenomeRNAi; 5133; -.
DR   PRO; PR:Q15116; -.
DR   Proteomes; UP000005640; Chromosome 2.
DR   Bgee; ENSG00000188389; -.
DR   CleanEx; HS_PDCD1; -.
DR   ExpressionAtlas; Q15116; baseline and differential.
DR   Genevisible; Q15116; HS.
DR   GO; GO:0009897; C:external side of plasma membrane; IEA:Ensembl.
DR   GO; GO:0016021; C:integral component of membrane; IEA:UniProtKB-KW.
DR   GO; GO:0005886; C:plasma membrane; TAS:Reactome.
DR   GO; GO:0004871; F:signal transducer activity; TAS:ProtInc.
DR   GO; GO:0006915; P:apoptotic process; TAS:ProtInc.
DR   GO; GO:0006959; P:humoral immune response; TAS:ProtInc.
DR   GO; GO:0007275; P:multicellular organism development; TAS:ProtInc.
DR   GO; GO:0043066; P:negative regulation of apoptotic process; IEA:Ensembl.
DR   GO; GO:0002644; P:negative regulation of tolerance induction; IEA:Ensembl.
DR   GO; GO:0070234; P:positive regulation of T cell apoptotic process; IDA:UniProtKB.
DR   GO; GO:0031295; P:T cell costimulation; TAS:Reactome.
DR   Gene3D; 2.60.40.10; -; 1.
DR   InterPro; IPR007110; Ig-like_dom.
DR   InterPro; IPR036179; Ig-like_dom_sf.
DR   InterPro; IPR013783; Ig-like_fold.
DR   InterPro; IPR003599; Ig_sub.
DR   InterPro; IPR013106; Ig_V-set.
DR   Pfam; PF07686; V-set; 1.
DR   SMART; SM00409; IG; 1.
DR   SMART; SM00406; IGv; 1.
DR   SUPFAM; SSF48726; SSF48726; 1.
DR   PROSITE; PS50835; IG_LIKE; 1.
PE   1: Evidence at protein level;
KW   3D-structure; Apoptosis; Complete proteome; Disulfide bond;
KW   Glycoprotein; Immunity; Immunoglobulin domain; Membrane; Polymorphism;
KW   Reference proteome; Signal; Systemic lupus erythematosus;
KW   Transmembrane; Transmembrane helix.
FT   SIGNAL        1     20       {ECO:0000255}.
FT   CHAIN        21    288       Programmed cell death protein 1.
FT                                /FTId=PRO_0000014892.
FT   TOPO_DOM     21    170       Extracellular. {ECO:0000255}.
FT   TRANSMEM    171    191       Helical. {ECO:0000255}.
FT   TOPO_DOM    192    288       Cytoplasmic. {ECO:0000255}.
FT   DOMAIN       35    145       Ig-like V-type.
FT   CARBOHYD     49     49       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD     58     58       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD     74     74       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   CARBOHYD    116    116       N-linked (GlcNAc...) asparagine.
FT                                {ECO:0000255}.
FT   DISULFID     54    123       {ECO:0000255|PROSITE-ProRule:PRU00114}.
FT   VARIANT     215    215       A -> V (in dbSNP:rs2227982).
FT                                /FTId=VAR_031685.
FT   CONFLICT     38     38       S -> F (in Ref. 2; AAC51773).
FT                                {ECO:0000305}.
FT   CONFLICT    162    162       P -> S (in Ref. 1; AAC41700).
FT                                {ECO:0000305}.
FT   STRAND       27     29       {ECO:0000244|PDB:5WT9}.
FT   STRAND       36     38       {ECO:0000244|PDB:5GGS}.
FT   STRAND       40     45       {ECO:0000244|PDB:5GGS}.
FT   STRAND       50     55       {ECO:0000244|PDB:5GGS}.
FT   STRAND       60     70       {ECO:0000244|PDB:5GGS}.
FT   STRAND       72     74       {ECO:0000244|PDB:4ZQK}.
FT   STRAND       76     83       {ECO:0000244|PDB:5GGS}.
FT   STRAND       95     99       {ECO:0000244|PDB:5GGS}.
FT   STRAND      103    112       {ECO:0000244|PDB:5GGS}.
FT   HELIX       115    117       {ECO:0000244|PDB:5GGS}.
FT   STRAND      119    131       {ECO:0000244|PDB:5GGS}.
FT   STRAND      134    136       {ECO:0000244|PDB:5GGS}.
FT   STRAND      140    145       {ECO:0000244|PDB:5GGS}.
SQ   SEQUENCE   288 AA;  31647 MW;  A5210FD40C304FB7 CRC64;
     MQIPQAPWPV VWAVLQLGWR PGWFLDSPDR PWNPPTFSPA LLVVTEGDNA TFTCSFSNTS
     ESFVLNWYRM SPSNQTDKLA AFPEDRSQPG QDCRFRVTQL PNGRDFHMSV VRARRNDSGT
     YLCGAISLAP KAQIKESLRA ELRVTERRAE VPTAHPSPSP RPAGQFQTLV VGVVGGLLGS
     LVLLVWVLAV ICSRAARGTI GARRTGQPLK EDPSAVPVFS VDYGELDFQW REKTPEPPVP
     CVPEQTEYAT IVFPSGMGTS SPARRGSADG PRSAQPLRPE DGHCSWPL
//|]

idAns :: ID
idAns = ID "PDCD1_HUMAN" Reviewed 288

acAns :: AC
acAns = AC ["Q15116", "O00517", "Q8IX89"]

dtAns :: DT
dtAns = DT "01-NOV-1997" "Swiss-Prot" "17-APR-2007" 3 "28-FEB-2018" 163

deAns :: DE
deAns = DE (Just (Name "Programmed cell death protein 1" ["Protein PD-1", "hPD-1"] []))
           [CDAntigen "CD279"] [] [] [] [Precursor]

de2Ans :: DE
de2Ans = DE (Just (Name "Arginine biosynthesis bifunctional protein argJ" [] [])) [] []
            [DE (Just (Name "Glutamate N-acetyltransferase" [] ["2.3.1.35"]))
                [Simple (Name "Ornithine acetyltransferase" ["OATase"] []),
                 Simple (Name "Ornithine transacetylase" [] [])]
                [] [] [] [],
             DE (Just (Name "Amino-acid acetyltransferase" [] ["2.3.1.1"]))
                [Simple (Name "N-acetylglutamate synthase" ["AGS"] [])]
                [] [] [] []
            ]
            [DE (Just (Name "Arginine biosynthesis bifunctional protein argJ alpha chain" [] []))
                [] [] [] [] [],
             DE (Just (Name "Arginine biosynthesis bifunctional protein argJ beta chain" [] []))
                [] [] [] [] []]
            []

gnAns :: [GN]
gnAns = [GN (Just "PDCD1") ["PD1"] [] []]

osAns :: OS
osAns = OS "Homo sapiens (Human)"

ogAns :: OG
ogAns = Plasmid ["R6-5", "IncFII R100 (NR1)", "IncFII R1-19 (R1 drd-19)"]

ocAns :: OC
ocAns = OC ["Eukaryota", "Metazoa", "Chordata", "Craniata",
            "Vertebrata", "Euteleostomi", "Mammalia", "Eutheria",
            "Euarchontoglires", "Primates", "Haplorrhini", "Catarrhini",
            "Hominidae", "Homo"]

ohAns :: OH
ohAns = OH "9536" "Cercopithecus hamlyni (Owl-faced monkey) (Hamlyn's monkey)"

oxAns :: OX
oxAns = OX "NCBI_TaxID" "9606"

refAns :: [Reference]
refAns = [Reference 1 "NUCLEOTIDE SEQUENCE [GENOMIC DNA]" [] [(PubMed,"7851902"),(DOI,"10.1006/geno.1994.1562")] [] ["Shinohara T.","Taniwaki M.","Ishida Y.","Kawaich M.","Honjo T."] (Just "Structure and chromosomal localization of the human PD-1 gene (PDCD1).") "Genomics 23:704-706(1994)",
          Reference 2 "NUCLEOTIDE SEQUENCE [MRNA]" [] [(PubMed,"9332365"),(DOI,"10.1016/S0378-1119(97)00260-6")] [] ["Finger L.R.","Pu J.","Wasserman R.","Vibhakar R.","Louie E.","Hardy R.R.","Burrows P.D.","Billips L.D."] (Just "The human PD-1 gene: complete cDNA, genomic organization, and developmentally regulated expression in B cell progenitors.") "Gene 197:177-187(1997)",
          Reference 3 "ERRATUM" [] [] [] ["Finger L.R.","Pu J.","Wasserman R.","Vibhakar R.","Louie E.","Hardy R.R.","Burrows P.D.","Billips L.D."] Nothing "Gene 203:253-253(1997)",
          Reference 4 "NUCLEOTIDE SEQUENCE [GENOMIC DNA], AND INVOLVEMENT IN SLEB2" [] [(PubMed,"12402038"),(DOI,"10.1038/ng1020")] [] ["Prokunina L.","Castillejo-Lopez C.","Oberg F.","Gunnarsson I.","Berg L.","Magnusson V.","Brookes A.J.","Tentler D.","Kristjansdottir H.","Grondal G.","Bolstad A.I.","Svenungsson E.","Lundberg I.","Sturfelt G.","Jonssen A.","Truedsson L.","Lima G.","Alcocer-Varela J.","Jonsson R.","Gyllensten U.B.","Harley J.B.","Alarcon-Segovia D.","Steinsson K.","Alarcon-Riquelme M.E."] (Just "A regulatory polymorphism in PDCD1 is associated with susceptibility to systemic lupus erythematosus in humans.") "Nat. Genet. 32:666-669(2002)",
          Reference 5 "NUCLEOTIDE SEQUENCE [MRNA]" [] [] [] ["He X.","Xu L.","Liu Y.","Zeng Y."] (Just "Cloning of PD-1 cDNA from activated peripheral leukocytes.") "Submitted (FEB-2003) to the EMBL/GenBank/DDBJ databases",
          Reference 6 "NUCLEOTIDE SEQUENCE [GENOMIC DNA]" [] [] [] ["Livingston R.J.","Shaffer T.","McFarland I.","Nguyen C.P.","Stanaway I.B.","Rajkumar N.","Johnson E.J.","da Ponte S.H.","Willa H.","Ahearn M.O.","Bertucci C.","Acklestad J.","Carroll A.","Swanson J.","Gildersleeve H.I.","Nickerson D.A."] Nothing "Submitted (OCT-2006) to the EMBL/GenBank/DDBJ databases",
          Reference 7 "NUCLEOTIDE SEQUENCE [LARGE SCALE MRNA]" [] [(PubMed,"14702039"),(DOI,"10.1038/ng1285")] [] ["Ota T.","Suzuki Y.","Nishikawa T.","Otsuki T.","Sugiyama T.","Irie R.","Wakamatsu A.","Hayashi K.","Sato H.","Nagai K.","Kimura K.","Makita H.","Sekine M.","Obayashi M.","Nishi T.","Shibahara T.","Tanaka T.","Ishii S.","Yamamoto J.","Saito K.","Kawai Y.","Isono Y.","Nakamura Y.","Nagahari K.","Murakami K.","Yasuda T.","Iwayanagi T.","Wagatsuma M.","Shiratori A.","Sudo H.","Hosoiri T.","Kaku Y.","Kodaira H.","Kondo H.","Sugawara M.","Takahashi M.","Kanda K.","Yokoi T.","Furuya T.","Kikkawa E.","Omura Y.","Abe K.","Kamihara K.","Katsuta N.","Sato K.","Tanikawa M.","Yamazaki M.","Ninomiya K.","Ishibashi T.","Yamashita H.","Murakawa K.","Fujimori K.","Tanai H.","Kimata M.","Watanabe M.","Hiraoka S.","Chiba Y.","Ishida S.","Ono Y.","Takiguchi S.","Watanabe S.","Yosida M.","Hotuta T.","Kusano J.","Kanehori K.","Takahashi-Fujii A.","Hara H.","Tanase T.-O.","Nomura Y.","Togiya S.","Komai F.","Hara R.","Takeuchi K.","Arita M.","Imose N.","Musashino K.","Yuuki H.","Oshima A.","Sasaki N.","Aotsuka S.","Yoshikawa Y.","Matsunawa H.","Ichihara T.","Shiohata N.","Sano S.","Moriya S.","Momiyama H.","Satoh N.","Takami S.","Terashima Y.","Suzuki O.","Nakagawa S.","Senoh A.","Mizoguchi H.","Goto Y.","Shimizu F.","Wakebe H.","Hishigaki H.","Watanabe T.","Sugiyama A.","Takemoto M.","Kawakami B.","Yamazaki M.","Watanabe K.","Kumagai A.","Itakura S.","Fukuzumi Y.","Fujimori Y.","Komiyama M.","Tashiro H.","Tanigami A.","Fujiwara T.","Ono T.","Yamada K.","Fujii Y.","Ozaki K.","Hirao M.","Ohmori Y.","Kawabata A.","Hikiji T.","Kobatake N.","Inagaki H.","Ikema Y.","Okamoto S.","Okitani R.","Kawakami T.","Noguchi S.","Itoh T.","Shigeta K.","Senba T.","Matsumura K.","Nakajima Y.","Mizuno T.","Morinaga M.","Sasaki M.","Togashi T.","Oyama M.","Hata H.","Watanabe M.","Komatsu T.","Mizushima-Sugano J.","Satoh T.","Shirai Y.","Takahashi Y.","Nakagawa K.","Okumura K.","Nagase T.","Nomura N.","Kikuchi H.","Masuho Y.","Yamashita R.","Nakai K.","Yada T.","Nakamura Y.","Ohara O.","Isogai T.","Sugano S."] (Just "Complete sequencing and characterization of 21,243 full-length human cDNAs.") "Nat. Genet. 36:40-45(2004)",
          Reference 8 "NUCLEOTIDE SEQUENCE [LARGE SCALE MRNA]" [] [(PubMed,"15489334"),(DOI,"10.1101/gr.2596504")] ["The MGC Project Team;"] [] (Just "The status, quality, and expansion of the NIH full-length cDNA project: the Mammalian Gene Collection (MGC).") "Genome Res. 14:2121-2127(2004)",
          Reference 9 "NUCLEOTIDE SEQUENCE [LARGE SCALE GENOMIC DNA]" [] [] [] ["Mural R.J.","Istrail S.","Sutton G.","Florea L.","Halpern A.L.","Mobarry C.M.","Lippert R.","Walenz B.","Shatkay H.","Dew I.","Miller J.R.","Flanigan M.J.","Edwards N.J.","Bolanos R.","Fasulo D.","Halldorsson B.V.","Hannenhalli S.","Turner R.","Yooseph S.","Lu F.","Nusskern D.R.","Shue B.C.","Zheng X.H.","Zhong F.","Delcher A.L.","Huson D.H.","Kravitz S.A.","Mouchard L.","Reinert K.","Remington K.A.","Clark A.G.","Waterman M.S.","Eichler E.E.","Adams M.D.","Hunkapiller M.W.","Myers E.W.","Venter J.C."] Nothing "Submitted (JUL-2005) to the EMBL/GenBank/DDBJ databases",
          Reference 10 "FUNCTION" [] [(PubMed,"21276005"),(DOI,"10.1111/j.1749-6632.2010.05919.x")] [] ["Fife B.T.","Pauken K.E."] (Just "The role of the PD-1 pathway in autoimmunity and peripheral tolerance.") "Ann. N. Y. Acad. Sci. 1217:45-59(2011)"]

ccAns :: [CC]
ccAns = [CC "FUNCTION" "Inhibitory cell surface receptor involved in the regulation of T-cell function during immunity and tolerance. Upon ligand binding, inhibits T-cell effector functions in an antigen-specific manner. Possible cell death inducer, in association with other factors.",
         CC "SUBUNIT" "Monomer.",
         CC "INTERACTION" "Q9NZQ7:CD274; NbExp=2; IntAct=EBI-4314328, EBI-4314282; Q9NZQ7-1:CD274; NbExp=2; IntAct=EBI-4314328, EBI-15686469; Q06124:PTPN11; NbExp=3; IntAct=EBI-4314328, EBI-297779;",
         CC "SUBCELLULAR LOCATION" "Membrane; Single-pass type I membrane protein.",
         CC "DEVELOPMENTAL STAGE" "Induced at programmed cell death.",
         CC "DISEASE" "Systemic lupus erythematosus 2 (SLEB2) [MIM:605218]: A chronic, relapsing, inflammatory, and often febrile multisystemic disorder of connective tissue, characterized principally by involvement of the skin, joints, kidneys and serosal membranes. It is of unknown etiology, but is thought to represent a failure of the regulatory mechanisms of the autoimmune system. The disease is marked by a wide range of system dysfunctions, an elevated erythrocyte sedimentation rate, and the formation of LE cells in the blood or bone marrow."]

drAns :: [DR]
drAns = [DR "EMBL" "L27440" ["AAC41700.1","-","Genomic_DNA"],
         DR "EMBL" "U64863" ["AAC51773.1","-","mRNA"],
         DR "EMBL" "AF363458" ["AAN64003.1","-","Genomic_DNA"],
         DR "EMBL" "AY238517" ["AAO63583.1","-","mRNA"],
         DR "EMBL" "EF064716" ["ABK41899.1","-","Genomic_DNA"],
         DR "EMBL" "AK313848" ["BAG36577.1","-","mRNA"],
         DR "EMBL" "CH471063" ["EAW71298.1","-","Genomic_DNA"],
         DR "EMBL" "BC074740" ["AAH74740.1","-","mRNA"],
         DR "CCDS" "CCDS33428.1" ["-"],
         DR "PIR" "A55737" ["A55737"],
         DR "RefSeq" "NP_005009.2" ["NM_005018.2"],
         DR "UniGene" "Hs.158297" ["-"],
         DR "PDB" "2M2D" ["NMR","-","A=34-150"],
         DR "PDB" "3RRQ" ["X-ray","2.10 A","A=32-160"],
         DR "PDB" "4ZQK" ["X-ray","2.45 A","B=33-150"],
         DR "PDB" "5B8C" ["X-ray","2.15 A","C/F/I/L=32-160"],
         DR "PDB" "5GGR" ["X-ray","3.30 A","Y/Z=26-150"],
         DR "PDB" "5GGS" ["X-ray","2.00 A","Y/Z=26-148"],
         DR "PDB" "5IUS" ["X-ray","2.89 A","A/B=26-146"],
         DR "PDB" "5JXE" ["X-ray","2.90 A","A/B=33-146"],
         DR "PDB" "5WT9" ["X-ray","2.40 A","G=1-167"],
         DR "PDBsum" "2M2D" ["-"],
         DR "PDBsum" "3RRQ" ["-"],
         DR "PDBsum" "4ZQK" ["-"],
         DR "PDBsum" "5B8C" ["-"],
         DR "PDBsum" "5GGR" ["-"],
         DR "PDBsum" "5GGS" ["-"],
         DR "PDBsum" "5IUS" ["-"],
         DR "PDBsum" "5JXE" ["-"],
         DR "PDBsum" "5WT9" ["-"],
         DR "ProteinModelPortal" "Q15116" ["-"],
         DR "SMR" "Q15116" ["-"],
         DR "BioGrid" "111160" ["61"],
         DR "DIP" "DIP-44126N" ["-"],
         DR "IntAct" "Q15116" ["5"],
         DR "MINT" "Q15116" ["-"],
         DR "STRING" "9606.ENSP00000335062" ["-"],
         DR "ChEMBL" "CHEMBL3307223" ["-"],
         DR "DrugBank" "DB05916" ["CT-011"],
         DR "DrugBank" "DB09035" ["Nivolumab"],
         DR "DrugBank" "DB09037" ["Pembrolizumab"],
         DR "GuidetoPHARMACOLOGY" "2760" ["-"],
         DR "iPTMnet" "Q15116" ["-"],
         DR "PhosphoSitePlus" "Q15116" ["-"],
         DR "BioMuta" "PDCD1" ["-"],
         DR "DMDM" "145559515" ["-"],
         DR "PaxDb" "Q15116" ["-"],
         DR "PeptideAtlas" "Q15116" ["-"],
         DR "PRIDE" "Q15116" ["-"],
         DR "DNASU" "5133" ["-"],
         DR "Ensembl" "ENST00000334409" ["ENSP00000335062","ENSG00000188389"],
         DR "Ensembl" "ENST00000618185" ["ENSP00000480684","ENSG00000276977"],
         DR "GeneID" "5133" ["-"],
         DR "KEGG" "hsa:5133" ["-"],
         DR "UCSC" "uc002wcq.5" ["human"],
         DR "CTD" "5133" ["-"],
         DR "DisGeNET" "5133" ["-"],
         DR "EuPathDB" "HostDB:ENSG00000188389.10" ["-"],
         DR "GeneCards" "PDCD1" ["-"],
         DR "H-InvDB" "HIX0030684" ["-"],
         DR "HGNC" "HGNC:8760" ["PDCD1"],
         DR "HPA" "CAB038418" ["-"],
         DR "HPA" "HPA035981" ["-"],
         DR "MalaCards" "PDCD1" ["-"],
         DR "MIM" "109100" ["phenotype"],
         DR "MIM" "600244" ["gene"],
         DR "MIM" "605218" ["phenotype"],
         DR "neXtProt" "NX_Q15116" ["-"],
         DR "OpenTargets" "ENSG00000188389" ["-"],
         DR "Orphanet" "802" ["Multiple sclerosis"],
         DR "Orphanet" "536" ["Systemic lupus erythematosus"],
         DR "PharmGKB" "PA33110" ["-"],
         DR "eggNOG" "ENOG410J26W" ["Eukaryota"],
         DR "eggNOG" "ENOG41116U6" ["LUCA"],
         DR "GeneTree" "ENSGT00390000013662" ["-"],
         DR "HOGENOM" "HOG000253959" ["-"],
         DR "HOVERGEN" "HBG053534" ["-"],
         DR "InParanoid" "Q15116" ["-"],
         DR "KO" "K06744" ["-"],
         DR "OMA" "DFQWREK" ["-"],
         DR "OrthoDB" "EOG091G0EE8" ["-"],
         DR "PhylomeDB" "Q15116" ["-"],
         DR "TreeFam" "TF336181" ["-"],
         DR "Reactome" "R-HSA-389948" ["PD-1 signaling"],
         DR "SIGNOR" "Q15116" ["-"],
         DR "ChiTaRS" "PDCD1" ["human"],
         DR "GeneWiki" "Programmed_cell_death_1" ["-"],
         DR "GenomeRNAi" "5133" ["-"],
         DR "PRO" "PR:Q15116" ["-"],
         DR "Proteomes" "UP000005640" ["Chromosome 2"],
         DR "Bgee" "ENSG00000188389" ["-"],
         DR "CleanEx" "HS_PDCD1" ["-"],
         DR "ExpressionAtlas" "Q15116" ["baseline and differential"],
         DR "Genevisible" "Q15116" ["HS"],
         DR "GO" "GO:0009897" ["C:external side of plasma membrane","IEA:Ensembl"],
         DR "GO" "GO:0016021" ["C:integral component of membrane","IEA:UniProtKB-KW"],
         DR "GO" "GO:0005886" ["C:plasma membrane","TAS:Reactome"],
         DR "GO" "GO:0004871" ["F:signal transducer activity","TAS:ProtInc"],
         DR "GO" "GO:0006915" ["P:apoptotic process","TAS:ProtInc"],
         DR "GO" "GO:0006959" ["P:humoral immune response","TAS:ProtInc"],
         DR "GO" "GO:0007275" ["P:multicellular organism development","TAS:ProtInc"],
         DR "GO" "GO:0043066" ["P:negative regulation of apoptotic process","IEA:Ensembl"],
         DR "GO" "GO:0002644" ["P:negative regulation of tolerance induction","IEA:Ensembl"],
         DR "GO" "GO:0070234" ["P:positive regulation of T cell apoptotic process","IDA:UniProtKB"],
         DR "GO" "GO:0031295" ["P:T cell costimulation","TAS:Reactome"],
         DR "Gene3D" "2.60.40.10" ["-","1"],
         DR "InterPro" "IPR007110" ["Ig-like_dom"],
         DR "InterPro" "IPR036179" ["Ig-like_dom_sf"],
         DR "InterPro" "IPR013783" ["Ig-like_fold"],
         DR "InterPro" "IPR003599" ["Ig_sub"],
         DR "InterPro" "IPR013106" ["Ig_V-set"],
         DR "Pfam" "PF07686" ["V-set","1"],
         DR "SMART" "SM00409" ["IG","1"],
         DR "SMART" "SM00406" ["IGv","1"],
         DR "SUPFAM" "SSF48726" ["SSF48726","1"],
         DR "PROSITE" "PS50835" ["IG_LIKE","1"]]

peAns :: PE
peAns = EvidenceAtProteinLevel

kwAns :: KW
kwAns = KW ["3D-structure", "Apoptosis", "Complete proteome", "Disulfide bond",
            "Glycoprotein", "Immunity", "Immunoglobulin domain", "Membrane", "Polymorphism",
            "Reference proteome", "Signal", "Systemic lupus erythematosus",
            "Transmembrane", "Transmembrane helix"]

ftAns :: [FT]
ftAns = [FT "SIGNAL"   (ExactEP 1)   (ExactEP 20)  [],
         FT "CHAIN"    (ExactEP 21)  (ExactEP 288) ["Programmed cell death protein 1","/FTId=PRO_0000014892"],
         FT "TOPO_DOM" (ExactEP 21)  (ExactEP 170) ["Extracellular"],
         FT "TRANSMEM" (ExactEP 171) (ExactEP 191) ["Helical"],
         FT "TOPO_DOM" (ExactEP 192) (ExactEP 288) ["Cytoplasmic"],
         FT "DOMAIN"   (ExactEP 35)  (ExactEP 145) ["Ig-like V-type"],
         FT "CARBOHYD" (ExactEP 49)  (ExactEP 49)  ["N-linked (GlcNAc...) asparagine"],
         FT "CARBOHYD" (ExactEP 58)  (ExactEP 58)  ["N-linked (GlcNAc...) asparagine"],
         FT "CARBOHYD" (ExactEP 74)  (ExactEP 74)  ["N-linked (GlcNAc...) asparagine"],
         FT "CARBOHYD" (ExactEP 116) (ExactEP 116) ["N-linked (GlcNAc...) asparagine"],
         FT "DISULFID" (ExactEP 54)  (ExactEP 123) [],
         FT "VARIANT"  (ExactEP 215) (ExactEP 215) ["A -> V (in dbSNP:rs2227982)","/FTId=VAR_031685"],
         FT "CONFLICT" (ExactEP 38)  (ExactEP 38)  ["S -> F (in Ref. 2; AAC51773)"],
         FT "CONFLICT" (ExactEP 162) (ExactEP 162) ["P -> S (in Ref. 1; AAC41700)"],
         FT "STRAND"   (ExactEP 27)  (ExactEP 29)  [],
         FT "STRAND"   (ExactEP 36)  (ExactEP 38)  [],
         FT "STRAND"   (ExactEP 40)  (ExactEP 45)  [],
         FT "STRAND"   (ExactEP 50)  (ExactEP 55)  [],
         FT "STRAND"   (ExactEP 60)  (ExactEP 70)  [],
         FT "STRAND"   (ExactEP 72)  (ExactEP 74)  [],
         FT "STRAND"   (ExactEP 76)  (ExactEP 83)  [],
         FT "STRAND"   (ExactEP 95)  (ExactEP 99)  [],
         FT "STRAND"   (ExactEP 103) (ExactEP 112) [],
         FT "HELIX"    (ExactEP 115) (ExactEP 117) [],
         FT "STRAND"   (ExactEP 119) (ExactEP 131) [],
         FT "STRAND"   (ExactEP 134) (ExactEP 136) [],
         FT "STRAND"   (ExactEP 140) (ExactEP 145) []]

sqAns :: SQ
sqAns = SQ 288 31647 "A5210FD40C304FB7"
           "MQIPQAPWPVVWAVLQLGWRPGWFLDSPDRPWNPPTFSPALLVVTEGDNATFTCSFSNTSESFVLNWYRMSPSNQTDKLAAFPEDRSQPGQDCRFRVTQLPNGRDFHMSVVRARRNDSGTYLCGAISLAPKAQIKESLRAELRVTERRAEVPTAHPSPSPRPAGQFQTLVVGVVGGLLGSLVLLVWVLAVICSRAARGTIGARRTGQPLKEDPSAVPVFSVDYGELDFQWREKTPEPPVPCVPEQTEYATIVFPSGMGTSSPARRGSADGPRSAQPLRPEDGHCSWPL"

endAns :: ()
endAns = ()

uniprotSectionSpec :: Spec
uniprotSectionSpec = 
  describe "Individual Uniprot sections parishioner" $ do
    it "parses ID lines" $
      parseOnly parseID idStr `shouldBe` Right idAns
    it "parses AC lines" $ 
      parseOnly parseAC acStr `shouldBe` Right acAns
    it "parses DT lines" $
      parseOnly parseDT dtStr `shouldBe` Right dtAns
    it "parses DE lines" $ do
      parseOnly parseDE deStr `shouldBe` Right deAns
      parseOnly parseDE de2Str `shouldBe` Right de2Ans
    it "parses GN lines" $
      parseOnly parseGN gnStr `shouldBe` Right gnAns
    it "parses OS lines" $
      parseOnly parseOS osStr `shouldBe` Right osAns
    it "parses OG lines (non-PD1)" $
      parseOnly parseOG ogStr `shouldBe` Right ogAns
    it "parses OC lines" $
      parseOnly parseOC ocStr `shouldBe` Right ocAns
    it "parses OX lines (non-PD1)" $
      parseOnly parseOX oxStr `shouldBe` Right oxAns
    it "parses OH lines (non-PD1)" $
      parseOnly parseOH ohStr `shouldBe` Right ohAns
    it "parses REF lines" $ do
      let parseManyRef = (:) <$> parseRef <*> many' (endOfLine *> parseRef)
      parseOnly parseManyRef refStr `shouldBe` Right refAns
    it "parses CC lines" $ do
      let parseManyCC = (:) <$> parseCC <*> many' (endOfLine *> parseCC)
      parseOnly parseManyCC ccStr `shouldBe` Right ccAns
    it "parses DR lines" $ do
      let parseManyDR = (:) <$> parseDR <*> many' (endOfLine *> parseDR)
      parseOnly parseManyDR drStr `shouldBe` Right drAns
    it "parses PE lines" $
      parseOnly parsePE peStr `shouldBe` Right peAns
    it "parses KW lines" $
      parseOnly parseKW kwStr `shouldBe` Right kwAns
    it "parses FT lines" $ do
      let parseManyFT = (:) <$> parseFT <*> many' (endOfLine *> parseFT)
      parseOnly parseManyFT ftStr `shouldBe` Right ftAns
    it "parses SQ lines" $ do
      parseOnly parseSQ sqStr `shouldBe` Right sqAns
    it "parses End lines" $
      parseOnly parseEnd endStr `shouldBe` Right endAns

uniprotFullSpec :: Spec
uniprotFullSpec =
  describe "Full Uniprot record parser" $ do
    it "parses PD1 example" $ do
      let record = Record idAns acAns dtAns deAns
                          gnAns osAns [] ocAns
                          oxAns [] refAns ccAns
                          drAns peAns (Just kwAns) ftAns sqAns
      parseOnly parseRecord pd1Str `shouldBe` Right record
