# clj-sound

FIXME: description

## Installation

Download from http://example.com/FIXME.

## Usage

FIXME: explanation

    $ java -jar clj-sound-0.1.0-standalone.jar [args]

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2019 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.

## Scratch

```
(defn sine [sample-rate freq]
  (let [term (/ 1 freq)
        samples (* term sample-rate)
        factor (/ (* 2 Math/PI) samples)]
    (map #(Math/sin (* % factor))
         (range samples))))

(defn amplitude [sample-size]
  (Math/pow 2 (- sample-size 1.1)))

(defn quantize [amplitude value]
  (int (* amplitude value)))
  
  
(defn pause [agent]
  (send agent assoc :playing false)
  (doto (:line @agent)
    .stop .flush))

(defn tone-freq [x]
  (-> (Math/pow 2 (/ x 11)) (* 440) (/ 512)))
  
https://github.com/josephwilk/finger-smudge/blob/master/src/finger_smudge/fft.clj

https://github.com/generateme/fastmath


```
