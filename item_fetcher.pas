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

uses item_fetcher_alternate,
     item_fetcher_amazon,
     item_fetcher_chipdip,
     item_fetcher_conrad,
     item_fetcher_ebay,
     item_fetcher_elecomp,
     item_fetcher_radiodetali;

class function TItemFetcher.createForSource(
    model: TModel;
    const source: string): TItemFetcher;
begin
  result := nil;
  // TODO(sergey): Commented out, HTTPS client gives 403.
  // if (result = nil) and (TItemFetcherAlternate.poll(source)) then begin
  //   result := TItemFetcherAlternate.Create(model);
  // end;
  if (result = nil) and (TItemFetcherAmazon.poll(source)) then begin
    result := TItemFetcherAmazon.Create(model);
  end;
  if (result = nil) and (TItemFetcherChipdip.poll(source)) then begin
    result := TItemFetcherChipdip.Create(model);
  end;
  // TODO(sergey): Disabled, for some reason HTML tag is empty.
  // if (result = nil) and (TItemFetcherConrad.poll(source)) then begin
  //   result := TItemFetcherConrad.Create(model);
  // end;
  if (result = nil) and (TItemFetcherEbay.poll(source)) then begin
    result := TItemFetcherEbay.Create(model);
  end;
  if (result = nil) and (TItemFetcherElecomp.poll(source)) then begin
    result := TItemFetcherElecomp.Create(model);
  end;
  // TODO(sergey): Commented out, parser can not parse the HTML page.
  // if (result = nil) and (TItemFetcherRadiodetali.poll(source)) then begin
  //   result := TItemFetcherRadiodetali.Create(model);
  // end;
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

