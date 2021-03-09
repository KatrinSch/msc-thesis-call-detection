# msc_thesis_call_detection
Detection of bird calls during nocturnal migration using machine learning (master thesis, University of Freiburg):

A random forest-based approach for the automatic detection of nocturnal flight calls (NFCs) in audio recordings.
The detection of NFCs is considered as a binary classification task, therefore continuous audio recordings are divided into short sound clips. A random forest classifier is trained with the aim of distinguishing whether NFCs are present in a sound clip or not.
For this purpose, acoustic features are extracted from sound clips. These are used as input for the random forest.
Its performance is evaluated by grouping audio recordings and applying a 'leave-one-group-out' cross-validation procedure.
The performance of the random forest classifier is further compared with the detection performance obtained by applying the ready-to-use version of BirdVoxDetect to the same audio recordings (https://github.com/BirdVox/birdvoxdetect).

Annotated audio recordings were provided by Ralph Martin (University of Freiburg, Germany). The recordings were made mainly in south-western Germany during 46 nights.
