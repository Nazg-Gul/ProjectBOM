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

unit item_fetcher_amazon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, item_fetcher_https, model, Dialogs;

type TItemFetcherAmazon = class(TItemFetcherHTTPS)
 public
  class function poll(const source: string): boolean; static;

 protected
  // Parse received page, return true on succes.
  function parsePage(const page: string;
                     var item: TModelItem): boolean; override;
end;

implementation

uses uriparser, util, sax_html, dom_html, dom;

class function TItemFetcherAmazon.poll(const source: string): boolean;
var uri: TURI;
begin
  result := false;
  uri := ParseURI(source);
  // TODO(sergey): What if it's amazon.lets.confuse.me.com domain?
  if uri.Host.StartsWith('amazon.') or
     uri.Host.StartsWith('www.amazon.') then begin
    result := true;
  end
end;

function TItemFetcherAmazon.parsePage(const page: string;
                                      var item: TModelItem): boolean;
var document: THTMLDocument;
    stream: TStringStream;
    node: TDOMNode;
    currency_string: string;
    image_source, image_filepath: string;
begin
  /////////////////////////////////////
  // Parse HTML from page.
  stream := TStringStream.create(page);
  ReadHTMLFile(document, stream);
  stream.Free();
  /////////////////////////////////////
  // Get into DOM itself.
  // Name.
  node := findElementById(document, 'productTitle');
  if node <> nil then begin
    item.name := trim(getNodePlainText(node));
  end;
  // Price
  node := findElementById(document, 'priceblock_ourprice');
  if node <> nil then begin
    item.price := getPriceAndCurrency(getNodePlainText(node), currency_string);
    item.currency := model_.getCurrencyByGuess(currency_string);
  end;
  // Image.
  node := findElementById(document, 'landingImage');
  if node <> nil then begin
    image_source := string(TDomElement(node).GetAttribute('src'));
    image_filepath := fetchFileToTemp(image_source);
    if image_filepath <> '' then begin
      item.loadImageFromFile(image_filepath);
      DeleteFile(image_filepath);
    end;
  end;
  /////////////////////////////////////
  // Set result flag.
  result := true;
  // We are done with all documents, free memory.
  document.Free();
end;

end.

