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

unit item_fetcher_https;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, item_fetcher, model, fphttpclient, dom_html, dom, util,
  Forms, LCLType;

type TItemFetcherHTTPS = class(TItemFetcher)
 public
  function fetch(const url: string; var item: TModelItem): boolean; override;
 protected
  // Parse received page, return true on succes.
  function parsePage(const url: string;
                     const page: string;
                     var item: TModelItem): boolean; virtual; abstract;

  // Standard HTML parser always returns nil for getElementByID().
  // So we implement own version, which just works!
  //
  // TODO(sergey): Find a better place for this.
  function findElementById(document: THTMLDocument;
                           const id: string): TDOMNode;
  function findElementByClass(document: THTMLDocument;
                              const class_name: string): TDOMNode;

  function findElementByAttribute(document: THTMLDocument;
                                  const attribute: string;
                                  const value: string): TDOMNode;
  function findElementByAttribute(nodes: TDOMNodeList;
                                  const attribute: string;
                                  const value: string): TDOMNode;
  function findElementByAttribute(node: TDOMNode;
                                  const attribute: string;
                                  const value: string): TDOMNode;

  // Similat to node.TextContent but does not include text from non-text
  // child nodes.
  function getNodePlainText(const node: TDOMNode;
                            recursive: boolean = false): string;
end;

implementation

function TItemFetcherHTTPS.fetch(const URL: string;
                                 var item: TModelItem): boolean;
var http_client: TFPHttpClient;
    received_data: string;
begin
  http_client := TFPHttpClient.Create(Nil);
  try
    http_client.AllowRedirect := true;
    http_client.AddHeader('User-Agent', 'Mozilla/5.0');
    http_client.AddHeader('Accept', '*/*');
    received_data := http_client.Get(url);
    result := parsePage(url, received_data, item);
  except
    on EHTTPClient do begin
      Application.MessageBox('Error fetching HTTP(s) page.',
                             'Error',
                             MB_ICONERROR);
    end;
  end;
  http_client.Free();
end;

////////////////////////////////////////////////////////////////////////////////
// Generic node finders.

function TItemFetcherHTTPS.findElementByAttribute(
        document: THTMLDocument;
        const attribute: string;
        const value: string): TDOMNode;
begin
  result := findElementByAttribute(document.ChildNodes, attribute, value);
end;

function TItemFetcherHTTPS.findElementByAttribute(
        nodes: TDOMNodeList;
        const attribute: string;
        const value: string): TDOMNode;
var i: integer;
    node, found_node: TDOMNode;
begin
  for i := 0 to nodes.Count - 1 do begin
    node := nodes.Item[i];
    found_node := findElementByAttribute(node, attribute, value);
    if found_node <> nil then begin
      result := found_node;
      exit;
    end;
  end;
  result := nil;
end;

function TItemFetcherHTTPS.findElementByAttribute(
        node: TDOMNode;
        const attribute: string;
        const value: string): TDOMNode;
begin
  if node.HasAttributes then begin
    if string(TDOMElement(node).GetAttribute(attribute)) = value then begin
      result := node;
      exit;
    end;
  end;
  if node.HasChildNodes then begin
    result := findElementByAttribute(node.ChildNodes, attribute, value);
  end else begin
    result := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Specific node finders.

function TItemFetcherHTTPS.findElementById(document: THTMLDocument;
                                           const id: string): TDOMNode;
begin
  result := findElementByAttribute(document.ChildNodes, 'id', id);
end;

function TItemFetcherHTTPS.findElementByClass(
    document: THTMLDocument;
    const class_name: string): TDOMNode;
begin
  result := findElementByAttribute(document.ChildNodes, 'class', class_name);
end;

function TItemFetcherHTTPS.getNodePlainText(const node: TDOMNode;
                                            recursive: boolean): string;
var i: integer;
    child_node: TDOMNode;
    current: string;
begin
  result := '';
  for i := 0 to node.ChildNodes.Count - 1 do begin
    child_node := node.ChildNodes.Item[i];
    current := '';
    if string(child_node.NodeName) = '#text' then begin
      current := string(child_node.TextContent);
    end else if child_node.HasChildNodes then begin
      if recursive then begin
        current := getNodePlainText(child_node);
      end;
    end;
    if current <> '' then begin
      if result <> '' then begin
        result += ' ';
      end;
      result += string(child_node.TextContent);
    end;
  end;
end;

end.

