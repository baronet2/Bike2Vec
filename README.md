# Bike2Vec: Vector Embedding Representations of Road Cycling Riders and Races

This is a companion repository to the paper "Bike2Vec: Vector Embedding Representations of Road Cycling Riders and Races",
presented at the [2023 MathSport International Conference](https://sites.google.com/view/mathsportinternational10) in Budapest, Hungary.


## Abstract

Vector embeddings have been successfully applied in several domains to obtain effective
representations of non-numeric data which can then be used in various downstream tasks. We
present a novel application of vector embeddings in professional road cycling by demonstrat-
ing a method to learn representations for riders and races based on historical results. We use
unsupervised learning techniques to validate that the resultant embeddings capture interesting
features of riders and races. These embeddings could be used for downstream prediction tasks
such as early talent identification and race outcome prediction.

## Usage

This repository contains code to reproduce the results from our paper.
The notebooks in `data_raw` scrape the necessary data and save CSV files in `data`.
The main code used to fit the embeddings and produce our validation results is in the `notebooks` folder.

## Citation

If you use this work, please cite our paper. We provide the `bibtex` below:

```bibtex
@inproceedings{bike2vec,
  author =       {Baron, Ethan and Janssens, Bram and Bogaert, Matthias},
  title =        {Bike2Vec: Vector Embedding Representations of Road
Cycling Riders and Races},
  booktitle =    {Proceedings of the 10th MathSport International Conference},
  year =         {2023},
  month =        {6}
}
```
