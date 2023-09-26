#!/bin/sh
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

set -e
raco test scsh/test.rkt
raco test alvs/test.rkt

for i in x86_64-win32 aarch64-macosx x86_64-macosx x86_64-linux;
do
  raco cross --target $i make git-gerrit.rkt;
  raco cross --target $i exe --orig-exe -o git-gerrit.$i git-gerrit.rkt ;
done

