CREATE CONSTRAINT ON (n:System) ASSERT n.name IS UNIQUE;
CREATE CONSTRAINT ON (n:System) ASSERT n.instance IS UNIQUE;
CREATE CONSTRAINT ON (n:System) ASSERT n.version IS UNIQUE;
//
CREATE CONSTRAINT ON (n:Database) ASSERT n.name IS UNIQUE;
//
CREATE CONSTRAINT ON (n:Synonym) ASSERT n.value IS UNIQUE;
CREATE INDEX ON :Synonym(value_up);
//
CREATE CONSTRAINT ON (n:Concept) ASSERT n.name IS UNIQUE;
// CREATE INDEX ON :Concept(name);
CREATE INDEX ON :Concept(shortID);
CREATE INDEX ON :Concept(label);
CREATE INDEX ON :Concept(label_up);
//CREATE INDEX ON :Concept(definition);
//CREATE INDEX ON :Concept(definition_up);
//
CREATE CONSTRAINT ON (n:Disease) ASSERT n.name IS UNIQUE;
CREATE INDEX ON :Disease(shortID);
CREATE INDEX ON :Disease(label);
CREATE INDEX ON :Disease(label_up);
//CREATE INDEX ON :Disease(definition);
//CREATE INDEX ON :Disease(definition_up);
//
CREATE CONSTRAINT ON (n:Phenotype) ASSERT n.name IS UNIQUE;
CREATE INDEX ON :Phenotype(shortID);
CREATE INDEX ON :Phenotype(label);
CREATE INDEX ON :Phenotype(label_up);
//CREATE INDEX ON :Phenotype(definition);
//CREATE INDEX ON :Phenotype(definition_up);
