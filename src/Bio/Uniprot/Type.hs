module Bio.Uniprot.Type where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | Which taxonomic 'kingdom' an
--   organism belongs to.
data Kingdom
  = Archea    -- ^ 'A' for archaea (=archaebacteria)
  | Bacteria  -- ^ 'B' for bacteria (=prokaryota or eubacteria)
  | Eukaryota -- ^ 'E' for eukaryota (=eukarya)
  | Virus     -- ^ 'V' for viruses and phages (=viridae)
  | Other     -- ^ 'O' for others (such as artificial sequences)
  deriving (Generic, Show, Eq, Ord, Bounded, Enum)

-- | Controlled vocabulary of species
data Organism = Organism
  { code         :: Text       -- ^ Code of organism (up to 5 symbols)
  , kingdom      :: Kingdom    -- ^ Kingdom of organism
  , officialName :: Text       -- ^ Official (scientific) name
  , commonName   :: Maybe Text -- ^ Common name
  , synonym      :: Maybe Text -- ^ Synonym name
  } deriving (Generic, Show, Eq, Ord)

-- | To distinguish the fully annotated entries in the Swiss-Prot
--   section of the UniProt Knowledgebase from the computer-annotated
--   entries in the TrEMBL section, the 'status' of each entry is
--   indicated in the first (ID) line of each entry
data Status
  = Reviewed   -- ^ Entries that have been manually reviewed and annotated by UniProtKB curators
  | Unreviewed -- ^ Computer-annotated entries that have not been reviewed by UniProtKB curators
  deriving (Generic, Show, Eq, Ord, Bounded, Enum)

-- | IDentification
data ID = ID
  { entryName :: Text   -- ^ This name is a useful means of identifying a sequence, but it is not a stable identifier as is the accession number.
  , status    :: Status -- ^ The status of the entry
  , seqLength :: Int    -- ^ The length of the molecule, which is the total number of amino acids in the sequence. This number includes the positions reported to be present but which have not been determined (coded as 'X').
  } deriving (Generic, Show, Eq, Ord)

-- | ACcession numbers.
--   The purpose of accession numbers is to provide a stable way of
--   identifying entries from release to release. It is sometimes
--   necessary for reasons of consistency to change the names of the
--   entries, for example, to ensure that related entries have similar
--   names. However, an accession number is always conserved, and
--   therefore allows unambiguous citation of entries.
--   Researchers who wish to cite entries in their publications should
--   always cite the first accession number. This is commonly referred
--   to as the 'primary accession number'. 'Secondary accession numbers'
--   are sorted alphanumerically.
newtype AC = AC
  { accessionNumbers :: [Text]
  } deriving (Generic, Show, Eq, Ord)

-- | DaTe: the date of creation and last modification of the database entry.
data DT = DT
  { dbIntegrationDate :: Text -- ^ Indicates when the entry first appeared in the database.
  , dbName            :: Text -- ^ Indicates in which section of UniProtKB, Swiss-Prot or TrEMBL, the entry can be found.
  , seqVersionDate    :: Text -- ^ Indicates when the sequence data was last modified.
  , seqVersion        :: Int  -- ^ The sequence version number of an entry is incremented by one when the amino acid sequence shown in the sequence record is modified.
  , entryVersionDate  :: Text -- ^ Indicates when data other than the sequence was last modified.
  , entryVersion      :: Int  -- ^ The entry version number is incremented by one whenever any data in the flat file representation of the entry is modified.
  } deriving (Generic, Show, Eq, Ord)

data Name = Name
  { fullName  :: Text   -- ^ The full name.
  , shortName :: [Text] -- ^ A set of abbreviations of the full name or acronyms.
  , ecNumber  :: [Text] -- ^ A set of Enzyme Commission numbers.
  } deriving (Generic, Show, Eq, Ord)

data AltName = Simple Name
             | Allergen Text
             | Biotech Text
             | CDAntigen Text
             | INN Text
  deriving (Generic, Show, Eq, Ord)

data Flag
  = Fragment  -- ^ The complete sequence is not determined.
  | Fragments -- ^ The complete sequence is not determined.
  | Precursor -- ^ The sequence displayed does not correspond to the mature form of the protein.
  deriving (Generic, Show, Read, Eq, Ord, Bounded, Enum)

-- | DEscription - general descriptive information about the sequence stored.
data DE = DE
  { recName  :: Maybe Name -- ^ The name recommended by the UniProt consortium.
  , altNames :: [AltName]  -- ^ A synonym of the recommended name.
  , subNames :: [Name]     -- ^ A name provided by the submitter of the underlying nucleotide sequence.
  , includes :: [DE]       -- ^ A protein is known to include multiple functional domains each of which is described by a different name.
  , contains :: [DE]       -- ^ The functional domains of an enzyme are cleaved, but the catalytic activity can only be observed, when the individual chains reorganize in a complex.
  , flags    :: [Flag]     -- ^ Flags whether the entire is a precursor or/and a fragment.
  } deriving (Generic, Show, Eq, Ord)

-- | Gene Name - the name(s) of the gene(s) that code for the stored protein sequence.
data GN = GN
  { geneName          :: Maybe Text -- ^ The name used to represent a gene.
  , synonyms          :: [Text]     -- ^ Other (unofficial) names of a gene.
  , orderedLocusNames :: [Text]     -- ^ A name used to represent an ORF in a completely sequenced genome or chromosome.
  , orfNames          :: [Text]     -- ^ A name temporarily attributed by a sequencing project to an open reading frame.
  } deriving (Generic, Show, Eq, Ord)

-- | Organism Species - the organism which was the source of the stored sequence.
newtype OS = OS
  { specie :: Text
  } deriving (Generic, Show, Eq, Ord)

-- | A enum of possible plastid types, based on either taxonomic
--   lineage or photosynthetic capacity.
data Plastid = PlastidSimple                  -- ^ The term Plastid is used when the capacities of the organism are unclear; for example in the parasitic plants of the Cuscuta lineage, where sometimes young tissue is photosynthetic.
             | PlastidApicoplast              -- ^ Apicoplasts are the plastids found in Apicocomplexa parasites such as Eimeria, Plasmodium and Toxoplasma; they are not photosynthetic.
             | PlastidChloroplast             -- ^ Chloroplasts are the plastids found in all land plants and algae with the exception of the glaucocystophyte algae (see below). Chloroplasts in green tissue are photosynthetic; in other tissues they may not be photosynthetic and then may also have secondary information relating to subcellular location (e.g. amyloplasts, chromoplasts).
             | PlastidOrganellarChromatophore -- ^ Chloroplasts are the plastids found in all land plants and algae with the exception of the glaucocystophyte algae (see below). Chloroplasts in green tissue are photosynthetic; in other tissues they may not be photosynthetic and then may also have secondary information relating to subcellular location (e.g. amyloplasts, chromoplasts).
             | PlastidCyanelle                -- ^ Cyanelles are the plastids found in the glaucocystophyte algae. They are also photosynthetic but their plastid has a vestigial cell wall between the 2 envelope membranes.
             | PlastidNonPhotosynthetic       -- ^ Non-photosynthetic plastid is used when the plastid in question derives from a photosynthetic lineage but the plastid in question is missing essential genes. Some examples are Aneura mirabilis, Epifagus virginiana, Helicosporidium (a liverwort, higher plant and green alga respectively).
  deriving (Generic, Show, Eq, Ord, Bounded, Enum)

-- | OrGanelle - indicates if the gene coding for a protein originates
--   from mitochondria, a plastid, a nucleomorph or a plasmid.
data OG = Hydrogenosome   -- ^ Hydrogenosomes are membrane-enclosed redox organelles found in some anaerobic unicellular eukaryotes which contain hydrogenase and produce hydrogen and ATP by glycolysis. They are thought to have evolved from mitochondria; most hydrogenosomes lack a genome, but some like (e.g. the anaerobic ciliate Nyctotherus ovalis) have retained a rudimentary genome.
        | Mitochondrion   -- ^ Mitochondria are redox-active membrane-bound organelles found in the cytoplasm of most eukaryotic cells. They are the site of sthe reactions of oxidative phosphorylation, which results in the formation of ATP.
        | Nucleomorph     -- ^ Nucleomorphs are reduced vestigal nuclei found in the plastids of cryptomonad and chlorachniophyte algae. The plastids originate from engulfed eukaryotic phototrophs.
        | Plasmid [Text]  -- ^ Plasmid with a specific name. If an entry reports the sequence of a protein identical in a number of plasmids, the names of these plasmids will all be listed.
        | Plastid Plastid -- ^ Plastids are classified based on either their taxonomic lineage or in some cases on their photosynthetic capacity.
  deriving (Generic, Show, Eq, Ord)

-- | Organism Classification - the taxonomic classification of the source organism.
newtype OC = OC
  { nodes :: [Text]
  } deriving (Generic, Show, Eq, Ord)

-- | Organism taxonomy cross-reference indicates the identifier of a
--   specific organism in a taxonomic database.
data OX = OX
  { databaseQualifier :: Text -- ^ Taxonomy database Qualifier
  , taxonomicCode     :: Text -- ^ Taxonomic code
  } deriving (Generic, Show, Eq, Ord)

-- | Organism Host - indicates the host organism(s) that are susceptible 
--   to be infected by a virus. Appears only in viral entries.
data OH = OH
  { taxId    :: Text -- ^
  , hostName :: Text -- ^
  } deriving (Generic, Show, Eq, Ord)

-- | Reference comment token.
data Token = STRAIN
           | PLASMID
           | TRANSPOSON
           | TISSUE
  deriving (Generic, Show, Eq, Ord, Bounded, Enum)

-- | Bibliographic database names.
data BibliographicDB = MEDLINE
                     | PubMed
                     | DOI
                     | AGRICOLA
  deriving (Generic, Show, Eq, Ord, Bounded, Enum)

-- | Reference Number - a sequential number to each reference citation in an entry.
data RN = RN
  { number   :: Int
  , evidence :: [Text]
  } deriving (Generic, Show, Eq, Ord)

-- | Reference lines.
data Reference = Reference
  { rn :: Int                       -- ^ Reference Number - a sequential number to each reference citation in an entry.
  , rp :: Text                      -- ^ Reference Position - the extent of the work relevant to the entry carried out by the authors.
  , rc :: [(Token, Text)]           -- ^ Reference Comment - comments relevant to the reference cited.
  , rx :: [(BibliographicDB, Text)] -- ^ Reference cross-reference - the identifier assigned to a specific reference in a bibliographic database.
  , rg :: [Text]                    -- ^ Reference Group - the consortium name associated with a given citation.
  , ra :: [Text]                    -- ^ Reference Author - authors of the paper (or other work) cited.
  , rt :: Maybe Text                -- ^ Reference Title - the title of the paper (or other work) cited as exactly as possible given the limitations of the computer character set.
  , rl :: Text                      -- ^ Reference Location - he conventional citation information for the reference.
  } deriving (Generic, Show, Eq, Ord)

-- | The comment blocks are arranged according to what we designate as 'topics'.
type Topic = Text

-- | Free text comments on the entry, and are used to convey any useful information.
data CC = CC
  { topic   :: Topic
  , comment :: Text
  } deriving (Generic, Show, Eq, Ord)

-- | Database cross-Reference - pointers to information in external
--   data resources that is related to UniProtKB entries.
data DR = DR
  { resourceAbbr :: Text   -- ^ The abbreviated name of the referenced resource (e.g. PDB).
  , resourceId   :: Text   -- ^ An unambiguous pointer to a record in the referenced resource.
  , optionalInfo :: [Text] -- ^ Used to provide optional information.
  } deriving (Generic, Show, Eq, Ord)

-- | Protein existence - indication on the evidences that we currently
--   have for the existence of a protein. Because most protein sequences
--   are derived from translation of nucleotide sequences and are mere
--   predictions, the PE line indicates what the evidences are of the
--   existence of a protein.
data PE = EvidenceAtProteinLevel
        | EvidenceAtTranscriptLevel
        | InferredFromHomology
        | Predicted
        | Uncertain
  deriving (Generic, Show, Eq, Ord)

-- | KeyWord - information that can be used to generate indexes of the
--   sequence entries based on functional, structural, or other categories.
newtype KW = KW
  { keywords :: [Text]
  } deriving (Generic, Show, Eq, Ord)

data Endpoint = ExactEP Int
              | NTerminalEP Int
              | CTerminalEP Int
              | UncertainEP Int
              | UnknownEP
  deriving (Generic, Show, Eq, Ord)

-- | Feature Table - means for the annotation of the sequence data.
data FT = FT
  { keyName     :: Text     -- ^ Key name.
  , fromEP      :: Endpoint -- ^ 'From' endpoint.
  , toEP        :: Endpoint -- ^ 'To' endpoint.
  , description :: [Text]   -- ^ Description.
  } deriving (Generic, Show, Eq, Ord)

-- | SeQuence header - sequence data and a quick summary of its content.
data SQ = SQ
  { length    :: Int  -- ^ Length of the sequence in amino acids.
  , molWeight :: Int  -- ^ Molecular weight rounded to the nearest mass unit (Dalton).
  , crc64     :: Text -- ^ Sequence 64-bit CRC (Cyclic Redundancy Check) value.
  , sequence  :: Text -- ^ Sequence of the protein
  } deriving (Generic, Show, Eq, Ord)

-- | Full UniProt record in UniProt-KB format.
data Record = Record
  { id   :: ID
  , ac   :: AC
  , dt   :: DT
  , de   :: DE
  , gn   :: [GN]
  , os   :: OS
  , og   :: [OG]
  , oc   :: OC
  , ox   :: OX
  , oh   :: [OH]
  , refs :: [Reference]
  , cc   :: [CC]
  , dr   :: [DR]
  , pe   :: PE
  , kw   :: Maybe KW
  , ft   :: [FT]
  , sq   :: SQ
  } deriving (Generic, Show, Eq, Ord)
