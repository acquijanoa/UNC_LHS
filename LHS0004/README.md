# Overview of the project

## Scripts 

### LHS000401.R
	Process the raw data


### Creating the DAG

Once the Snakemake workflow has been completed, a DAG graph can be created as follows, 

```
snakemake --dag | dot -Tpng > dag.png
```

It will save a file dat.png in the main folder.
