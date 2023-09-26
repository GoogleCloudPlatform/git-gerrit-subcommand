# Copyright 2023 Google LLC
# Author: Jun Sheng
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

FROM debian:10
RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*
WORKDIR /tmp/instlr
RUN curl -L -o racket-8.10-x86_64-linux-cs.sh\
    https://download.racket-lang.org/releases/8.10/installers/racket-8.10-x86_64-linux-cs.sh && \
    echo 497f0f518fa4c5f8529269580a6a79366f21315af26cf3ed1ae8b47e05ceac72 racket-8.10-x86_64-linux-cs.sh|\
    sha256sum -c - &&\
    bash racket-8.10-x86_64-linux-cs.sh --unix-style --dest /usr/ --create-dir 
RUN raco pkg install --auto --skip-installed  raco-cross ||true

WORKDIR /app

