# clarifai-hs
Haskell API Client for <a href="http://www.clarifai.com">Clarifai</a>.
This is the readme for the V2 API client. Click [here](https://github.com/caneroj1/clarifai-hs/tree/v2-refactor/src/Network/Clarifai/V1/README.md) for the V1 API readme.

Documentation hosted on <a href="https://hackage.haskell.org/package/clarifai">Hackage</a>.

Clarifai provides image/video recognition services, and this library provides a web client to their API.

## Usage

## Creating Inputs

Inputs to the Clarifai API can be constructed as follows:

```haskell

i :: Input
i = input (Url "img.png") & crop ?~ Crop 0.2 0.4 0.6 0.8
                          & allowDuplicate .~ True
                          & metadata .~ [("MyMeta", String "data")]

```

An `Input` data type is just a collection of relevant data about an image. The above example does the following:

* Creates an input from a url to an image
* Species crop settings for the image
* Tells Clarifai to allow multiple inputs with the same url
* Creates a metadata object with custom fields.

### Authorization
First create an ```App``` and then call the ```authorize``` function to authenticate your application. You must provide your client id and client secret.
```haskell

import Network.Clarifai
import Data.Either

clientID = "id"
clientSecret = "secret"

main = do
  let myApp = App clientID clientSecret
  resp <- authorize myApp

```
Then you can check the response for errors, or use the access token it provides back in the form of a ```Client```.

### Getting API Info
```haskell
-- Omitted previous code
main = do
  -- Be sure to check for errors
  let client = head $ rights [resp]
  infoResponse <- info client
```

If there are no errors, you will get back an ```Info``` containing various information about the statistics and limits of the Clarifai API, like maximum video size, maximum image size, etc. You can use this verify any files you may want to tag, before you send a tagging request.

### Verification
When going to verify batch sizes, it is important not to mix and match between videos and images. The batch sizes for images and videos are different, so you cannot verify the sizes of the batches together even though you can send them in the same request. I don't do any differentiating between what kind of file the ```FilePath``` points to.

If you are calling the ```verifyFiles``` function, however, you can have a list of both videos AND images, as long as the extensions of each ```FilePath``` indicate the type of the file.
```haskell
-- Omitted previous code
main = do
  -- Be sure to check for errors
  let apiInfo = head $ rights [infoResponse]
  let filePaths = [filePath1, filePath2, filePath3]

  -- Verify the image batch size
  -- The return value is a Bool indicating whether your list is of appropriate size for the API.
  verifyImageBatchSize apiInfo filePaths

  -- Verify the sizes of the files in the list. As long as each FilePath indicates whether the file is a video or an image,
  -- the size of the file will be checked to make sure it is within the appropriate bounds according to the API Info.
  verifyFiles infoData fileNames
```

### Tagging
We can send a tag request with our list of ```FilePath```s. I've omitted any steps where we may have filtered out which files
```verifyFiles``` marked as inappropriate for the request.
```haskell
-- Omitted previous code
tagResponse <- tag client fileNames
```

## Contributing
I'll gladly take contributions/improvements/pull requests.

## License
The MIT License (MIT)

Copyright (c) 2015 Joe Canero

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
