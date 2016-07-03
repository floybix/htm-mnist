# htm-mnist

Using MNIST to challenge standard HTM and motivate development of the algorithms.

## Usage


```
git clone
cd htm-mnist
mkdir public/data
cd public/data
curl http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz >train-images-idx3-ubyte.gz
curl http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz >train-labels-idx1-ubyte.gz
gunzip *.gz
cd ..
lein repl
etc
```

## Results

The reported NuPIC result of 5% error is purely due to the use of a
KNN classifier.  This is backed up by noting the KNN result listed on
http://yann.lecun.com/exdb/mnist/.

It is possible to do classification within HTM using a second layer,
with one (or potentially more) column per category that is forced to
be on and learning. I think this makes sense when we want to discover
what the HTM knows internally: we are asking if an HTM neuron can
learn to classify (match) a target pattern.

The classification done this way improves when we allow multiple
proximal segments, because they can capture different variants of the
classified category. (e.g. different forms of writing the number
"4"). Alternatively we could allow multiple columns on per category,
but it's easier to do it with segments.

I couldn't get better than ~ 20% classification error. I think this is
just how far you can get by matching simple shapes with lots of shared
pixels: simple shapes like the majority of "1"s, "6"s, "8"s. There is
a lot of remaining variation that is not classified together.

There is a missing mechanism for abstracting over all these variants
and over spatial deformations (translation, shear etc) even of an
identical shape.

Look at the receptive field plots of classification-layer columns. You
can see that some of the segments look like a different digit;
e.g. something that looks like "0" as a segment on the "9"
column. What's the deal with that? Well, there is no confusion at the
classification layer because we have forced the 1 target column to be
on. So instead it means that a lot of the same columns are coming on
in the first layer for both "0"s and "9"s -- for some variants of "0"s
and "9"s. And that implies that there is a lot of overlap in pixels
between those shapes. Of course -- some roundish "9"s would have a lot
of overlapping pixels with similarly roundish "0"s. Same goes for some
5s vs 6s, some 4s v 9s, some 2s vs 0s, etc. HTM sees the overlap and
matches them; there is no way to represent that a 9 must have a gap at
the right point, or that it must have an internal line join.

Local receptive fields seem like they should do better but
don't... again I think we're missing a way to learn invariance under
spatial deformations.


## License

Copyright © 2016 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
