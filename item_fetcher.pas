// Copyright (c) 2017, ProjectBOM authors

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.

// Author: Sergey Sharybin (sergey.vfx@gmail.com)

unit item_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, model;

type TItemFetcher = class
 public
  constructor Create(model: TModel);

  // Checkj whether this fetcher can handle given source.
  class function poll(const source: string): boolean; static;

  class function createForSource(
      model: TModel;
      const source: string): TItemFetcher; static;

  // Fetch all possible fields from give nsource to a given item.
  // Return true if fetch succeeded.
  function fetch(const source: string;
                 var item: TModelItem): boolean; virtual; abstract;
 protected
  model_: TModel;
end;

implementation

uses item_fetcher_amazon, item_fetcher_ebay;

class function TItemFetcher.createForSource(
    model: TModel;
    const source: string): TItemFetcher;
begin
  result := nil;
  if (result = nil) and (TItemFetcherAmazon.poll(source)) then begin
    result := TItemFetcherAmazon.Create(model);
  end;
  if (result = nil) and (TItemFetcherEbay.poll(source)) then begin
    result := TItemFetcherEbay.Create(model);
  end;
end;

constructor TItemFetcher.Create(model: TModel);
begin
  model_ := model;
end;

class function TItemFetcher.poll(const source: string): boolean;
begin
  result := false;
end;

end.
