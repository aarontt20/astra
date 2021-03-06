(* UTF-8 and HTML entities *)
let characters_htmlentities_descriptions =
(* data extracted from http://www.w3schools.com/
   on December, 18th, 2013 *)
[
" ",
"&#32;",
"space";
"!",
"&#33;",
"exclamation mark";
"\"",
"&#34;",
"quotation mark";
"#",
"&#35;",
"number sign";
"$",
"&#36;",
"dollar sign";
"%",
"&#37;",
"percent sign";
"&amp;",
"&#38;",
"ampersand";
"'",
"&#39;",
"apostrophe";
"(",
"&#40;",
"left parenthesis";
")",
"&#41;",
"right parenthesis";
"*",
"&#42;",
"asterisk";
"+",
"&#43;",
"plus sign";
",",
"&#44;",
"comma";
"-",
"&#45;",
"hyphen";
".",
"&#46;",
"period";
"/",
"&#47;",
"slash";
"0",
"&#48;",
"digit 0";
"1",
"&#49;",
"digit 1";
"2",
"&#50;",
"digit 2";
"3",
"&#51;",
"digit 3";
"4",
"&#52;",
"digit 4";
"5",
"&#53;",
"digit 5";
"6",
"&#54;",
"digit 6";
"7",
"&#55;",
"digit 7";
"8",
"&#56;",
"digit 8";
"9",
"&#57;",
"digit 9";
":",
"&#58;",
"colon";
";",
"&#59;",
"semicolon";
"&lt;",
"&#60;",
"less-than";
"=",
"&#61;",
"equals-to";
"&gt;",
"&#62;",
"greater-than";
"?",
"&#63;",
"question mark";
"@",
"&#64;",
"at sign";
"A",
"&#65;",
"uppercase A";
"B",
"&#66;",
"uppercase B";
"C",
"&#67;",
"uppercase C";
"D",
"&#68;",
"uppercase D";
"E",
"&#69;",
"uppercase E";
"F",
"&#70;",
"uppercase F";
"G",
"&#71;",
"uppercase G";
"H",
"&#72;",
"uppercase H";
"I",
"&#73;",
"uppercase I";
"J",
"&#74;",
"uppercase J";
"K",
"&#75;",
"uppercase K";
"L",
"&#76;",
"uppercase L";
"M",
"&#77;",
"uppercase M";
"N",
"&#78;",
"uppercase N";
"O",
"&#79;",
"uppercase O";
"P",
"&#80;",
"uppercase P";
"Q",
"&#81;",
"uppercase Q";
"R",
"&#82;",
"uppercase R";
"S",
"&#83;",
"uppercase S";
"T",
"&#84;",
"uppercase T";
"U",
"&#85;",
"uppercase U";
"V",
"&#86;",
"uppercase V";
"W",
"&#87;",
"uppercase W";
"X",
"&#88;",
"uppercase X";
"Y",
"&#89;",
"uppercase Y";
"Z",
"&#90;",
"uppercase Z";
"[",
"&#91;",
"left square bracket";
"\\",
"&#92;",
"backslash";
"]",
"&#93;",
"right square bracket";
"^",
"&#94;",
"caret";
"_",
"&#95;",
"underscore";
"`",
"&#96;",
"grave accent";
"a",
"&#97;",
"lowercase a";
"b",
"&#98;",
"lowercase b";
"c",
"&#99;",
"lowercase c";
"d",
"&#100;",
"lowercase d";
"e",
"&#101;",
"lowercase e";
"f",
"&#102;",
"lowercase f";
"g",
"&#103;",
"lowercase g";
"h",
"&#104;",
"lowercase h";
"i",
"&#105;",
"lowercase i";
"j",
"&#106;",
"lowercase j";
"k",
"&#107;",
"lowercase k";
"l",
"&#108;",
"lowercase l";
"m",
"&#109;",
"lowercase m";
"n",
"&#110;",
"lowercase n";
"o",
"&#111;",
"lowercase o";
"p",
"&#112;",
"lowercase p";
"q",
"&#113;",
"lowercase q";
"r",
"&#114;",
"lowercase r";
"s",
"&#115;",
"lowercase s";
"t",
"&#116;",
"lowercase t";
"u",
"&#117;",
"lowercase u";
"v",
"&#118;",
"lowercase v";
"w",
"&#119;",
"lowercase w";
"x",
"&#120;",
"lowercase x";
"y",
"&#121;",
"lowercase y";
"z",
"&#122;",
"lowercase z";
"{",
"&#123;",
"left curly brace";
"|",
"&#124;",
"vertical bar";
"}",
"&#125;",
"right curly brace";
"~",
"&#126;",
"tilde";
"\000",
"&#00;",
"null character";
"\001",
"&#01;",
"start of header";
"\002",
"&#02;",
"start of text";
"\003",
"&#03;",
"end of text";
"\004",
"&#04;",
"end of transmission";
"\005",
"&#05;",
"enquiry";
"\006",
"&#06;",
"acknowledge";
"\007",
"&#07;",
"bell (ring)";
"\008",
"&#08;",
"backspace";
"\009",
"&#09;",
"horizontal tab";
"\010",
"&#10;",
"line feed";
"\011",
"&#11;",
"vertical tab";
"\012",
"&#12;",
"form feed";
"\013",
"&#13;",
"carriage return";
"\014",
"&#14;",
"shift out";
"\015",
"&#15;",
"shift in";
"\016",
"&#16;",
"data link escape";
"\017",
"&#17;",
"device control 1";
"\018",
"&#18;",
"device control 2";
"\019",
"&#19;",
"device control 3";
"\020",
"&#20;",
"device control 4";
"\021",
"&#21;",
"negative acknowledge";
"\022",
"&#22;",
"synchronize";
"\023",
"&#23;",
"end transmission block";
"\024",
"&#24;",
"cancel";
"\025",
"&#25;",
"end of medium";
"\026",
"&#26;",
"substitute";
"\027",
"&#27;",
"escape";
"\028",
"&#28;",
"file separator";
"\029",
"&#29;",
"group separator";
"\030",
"&#30;",
"record separator";
"\031",
"&#31;",
"unit separator";
"\127",
"&#127;",
"delete (rubout)";
"\"",
"&quot;",
"quotation mark";
"'",
"&apos;",
"apostrophe";
"&",
"&amp;",
"ampersand";
"<",
"&lt;",
"less-than";
">",
"&gt;",
"greater-than";
"\xc2\xa0",
"&nbsp;",
"non-breaking space";
"\xc2\xa0",
"&#160;",
"non-breaking space";
"??",
"&iexcl;",
"inverted exclamation mark";
"??",
"&#161;",
"inverted exclamation mark";
"??",
"&cent;",
"cent";
"??",
"&#162;",
"cent";
"??",
"&pound;",
"pound";
"??",
"&#163;",
"pound";
"??",
"&curren;",
"currency";
"??",
"&#164;",
"currency";
"??",
"&yen;",
"yen";
"??",
"&#165;",
"yen";
"??",
"&brvbar;",
"broken vertical bar";
"??",
"&#166;",
"broken vertical bar";
"??",
"&sect;",
"section";
"??",
"&#167;",
"section";
"??",
"&uml;",
"spacing diaeresis";
"??",
"&#168;",
"spacing diaeresis";
"??",
"&copy;",
"copyright";
"??",
"&#169;",
"copyright";
"??",
"&ordf;",
"feminine ordinal indicator";
"??",
"&#170;",
"feminine ordinal indicator";
"??",
"&laquo;",
"angle quotation mark (left)";
"??",
"&#171;",
"angle quotation mark (left)";
"??",
"&not;",
"negation";
"??",
"&#172;",
"negation";
"?????",
"&shy;",
"soft hyphen";
"?????",
"&#173;",
"soft hyphen";
"??",
"&reg;",
"registered trademark";
"??",
"&#174;",
"registered trademark";
"??",
"&macr;",
"spacing macron";
"??",
"&#175;",
"spacing macron";
"??",
"&deg;",
"degree";
"??",
"&#176;",
"degree";
"??",
"&plusmn;",
"plus-or-minus";
"??",
"&#177;",
"plus-or-minus";
"??",
"&sup2;",
"superscript 2";
"??",
"&#178;",
"superscript 2";
"??",
"&sup3;",
"superscript 3";
"??",
"&#179;",
"superscript 3";
"??",
"&acute;",
"spacing acute";
"??",
"&#180;",
"spacing acute";
"??",
"&micro;",
"micro";
"??",
"&#181;",
"micro";
"??",
"&para;",
"paragraph";
"??",
"&#182;",
"paragraph";
"??",
"&middot;",
"middle dot";
"??",
"&#183;",
"middle dot";
"??",
"&cedil;",
"spacing cedilla";
"??",
"&#184;",
"spacing cedilla";
"??",
"&sup1;",
"superscript 1";
"??",
"&#185;",
"superscript 1";
"??",
"&ordm;",
"masculine ordinal indicator";
"??",
"&#186;",
"masculine ordinal indicator";
"??",
"&raquo;",
"angle quotation mark (right)";
"??",
"&#187;",
"angle quotation mark (right)";
"??",
"&frac14;",
"fraction 1/4";
"??",
"&#188;",
"fraction 1/4";
"??",
"&frac12;",
"fraction 1/2";
"??",
"&#189;",
"fraction 1/2";
"??",
"&frac34;",
"fraction 3/4";
"??",
"&#190;",
"fraction 3/4";
"??",
"&iquest;",
"inverted question mark";
"??",
"&#191;",
"inverted question mark";
"??",
"&times;",
"multiplication";
"??",
"&#215;",
"multiplication";
"??",
"&divide;",
"division";
"??",
"&#247;",
"division";
"??",
"&Agrave;",
"capital a, grave accent";
"??",
"&#192;",
"capital a, grave accent";
"??",
"&Aacute;",
"capital a, acute accent";
"??",
"&#193;",
"capital a, acute accent";
"??",
"&Acirc;",
"capital a, circumflex accent";
"??",
"&#194;",
"capital a, circumflex accent";
"??",
"&Atilde;",
"capital a, tilde";
"??",
"&#195;",
"capital a, tilde";
"??",
"&Auml;",
"capital a, umlaut mark";
"??",
"&#196;",
"capital a, umlaut mark";
"??",
"&Aring;",
"capital a, ring";
"??",
"&#197;",
"capital a, ring";
"??",
"&AElig;",
"capital ae";
"??",
"&#198;",
"capital ae";
"??",
"&Ccedil;",
"capital c, cedilla";
"??",
"&#199;",
"capital c, cedilla";
"??",
"&Egrave;",
"capital e, grave accent";
"??",
"&#200;",
"capital e, grave accent";
"??",
"&Eacute;",
"capital e, acute accent";
"??",
"&#201;",
"capital e, acute accent";
"??",
"&Ecirc;",
"capital e, circumflex accent";
"??",
"&#202;",
"capital e, circumflex accent";
"??",
"&Euml;",
"capital e, umlaut mark";
"??",
"&#203;",
"capital e, umlaut mark";
"??",
"&Igrave;",
"capital i, grave accent";
"??",
"&#204;",
"capital i, grave accent";
"??",
"&Iacute;",
"capital i, acute accent";
"??",
"&#205;",
"capital i, acute accent";
"??",
"&Icirc;",
"capital i, circumflex accent";
"??",
"&#206;",
"capital i, circumflex accent";
"??",
"&Iuml;",
"capital i, umlaut mark";
"??",
"&#207;",
"capital i, umlaut mark";
"??",
"&ETH;",
"capital eth, Icelandic";
"??",
"&#208;",
"capital eth, Icelandic";
"??",
"&Ntilde;",
"capital n, tilde";
"??",
"&#209;",
"capital n, tilde";
"??",
"&Ograve;",
"capital o, grave accent";
"??",
"&#210;",
"capital o, grave accent";
"??",
"&Oacute;",
"capital o, acute accent";
"??",
"&#211;",
"capital o, acute accent";
"??",
"&Ocirc;",
"capital o, circumflex accent";
"??",
"&#212;",
"capital o, circumflex accent";
"??",
"&Otilde;",
"capital o, tilde";
"??",
"&#213;",
"capital o, tilde";
"??",
"&Ouml;",
"capital o, umlaut mark";
"??",
"&#214;",
"capital o, umlaut mark";
"??",
"&Oslash;",
"capital o, slash";
"??",
"&#216;",
"capital o, slash";
"??",
"&Ugrave;",
"capital u, grave accent";
"??",
"&#217;",
"capital u, grave accent";
"??",
"&Uacute;",
"capital u, acute accent";
"??",
"&#218;",
"capital u, acute accent";
"??",
"&Ucirc;",
"capital u, circumflex accent";
"??",
"&#219;",
"capital u, circumflex accent";
"??",
"&Uuml;",
"capital u, umlaut mark";
"??",
"&#220;",
"capital u, umlaut mark";
"??",
"&Yacute;",
"capital y, acute accent";
"??",
"&#221;",
"capital y, acute accent";
"??",
"&THORN;",
"capital THORN, Icelandic";
"??",
"&#222;",
"capital THORN, Icelandic";
"??",
"&szlig;",
"small sharp s, German";
"??",
"&#223;",
"small sharp s, German";
"??",
"&agrave;",
"small a, grave accent";
"??",
"&#224;",
"small a, grave accent";
"??",
"&aacute;",
"small a, acute accent";
"??",
"&#225;",
"small a, acute accent";
"??",
"&acirc;",
"small a, circumflex accent";
"??",
"&#226;",
"small a, circumflex accent";
"??",
"&atilde;",
"small a, tilde";
"??",
"&#227;",
"small a, tilde";
"??",
"&auml;",
"small a, umlaut mark";
"??",
"&#228;",
"small a, umlaut mark";
"??",
"&aring;",
"small a, ring";
"??",
"&#229;",
"small a, ring";
"??",
"&aelig;",
"small ae";
"??",
"&#230;",
"small ae";
"??",
"&ccedil;",
"small c, cedilla";
"??",
"&#231;",
"small c, cedilla";
"??",
"&egrave;",
"small e, grave accent";
"??",
"&#232;",
"small e, grave accent";
"??",
"&eacute;",
"small e, acute accent";
"??",
"&#233;",
"small e, acute accent";
"??",
"&ecirc;",
"small e, circumflex accent";
"??",
"&#234;",
"small e, circumflex accent";
"??",
"&euml;",
"small e, umlaut mark";
"??",
"&#235;",
"small e, umlaut mark";
"??",
"&igrave;",
"small i, grave accent";
"??",
"&#236;",
"small i, grave accent";
"??",
"&iacute;",
"small i, acute accent";
"??",
"&#237;",
"small i, acute accent";
"??",
"&icirc;",
"small i, circumflex accent";
"??",
"&#238;",
"small i, circumflex accent";
"??",
"&iuml;",
"small i, umlaut mark";
"??",
"&#239;",
"small i, umlaut mark";
"??",
"&eth;",
"small eth, Icelandic";
"??",
"&#240;",
"small eth, Icelandic";
"??",
"&ntilde;",
"small n, tilde";
"??",
"&#241;",
"small n, tilde";
"??",
"&ograve;",
"small o, grave accent";
"??",
"&#242;",
"small o, grave accent";
"??",
"&oacute;",
"small o, acute accent";
"??",
"&#243;",
"small o, acute accent";
"??",
"&ocirc;",
"small o, circumflex accent";
"??",
"&#244;",
"small o, circumflex accent";
"??",
"&otilde;",
"small o, tilde";
"??",
"&#245;",
"small o, tilde";
"??",
"&ouml;",
"small o, umlaut mark";
"??",
"&#246;",
"small o, umlaut mark";
"??",
"&oslash;",
"small o, slash";
"??",
"&#248;",
"small o, slash";
"??",
"&ugrave;",
"small u, grave accent";
"??",
"&#249;",
"small u, grave accent";
"??",
"&uacute;",
"small u, acute accent";
"??",
"&#250;",
"small u, acute accent";
"??",
"&ucirc;",
"small u, circumflex accent";
"??",
"&#251;",
"small u, circumflex accent";
"??",
"&uuml;",
"small u, umlaut mark";
"??",
"&#252;",
"small u, umlaut mark";
"??",
"&yacute;",
"small y, acute accent";
"??",
"&#253;",
"small y, acute accent";
"??",
"&thorn;",
"small thorn, Icelandic";
"??",
"&#254;",
"small thorn, Icelandic";
"??",
"&yuml;",
"small y, umlaut mark";
"??",
"&#255;",
"small y, umlaut mark";
"???",
"&forall;",
"for all";
"???",
"&#8704;",
"for all";
"???",
"&part;",
"part";
"???",
"&#8706;",
"part";
"???",
"&exist;",
"exists";
"???",
"&#8707;",
"exists";
"???",
"&empty;",
"empty";
"???",
"&#8709;",
"empty";
"???",
"&nabla;",
"nabla";
"???",
"&#8711;",
"nabla";
"???",
"&isin;",
"isin";
"???",
"&#8712;",
"isin";
"???",
"&notin;",
"notin";
"???",
"&#8713;",
"notin";
"???",
"&ni;",
"ni";
"???",
"&#8715;",
"ni";
"???",
"&prod;",
"prod";
"???",
"&#8719;",
"prod";
"???",
"&sum;",
"sum";
"???",
"&#8721;",
"sum";
"???",
"&minus;",
"minus";
"???",
"&#8722;",
"minus";
"???",
"&lowast;",
"lowast";
"???",
"&#8727;",
"lowast";
"???",
"&radic;",
"square root";
"???",
"&#8730;",
"square root";
"???",
"&prop;",
"proportional to";
"???",
"&#8733;",
"proportional to";
"???",
"&infin;",
"infinity";
"???",
"&#8734;",
"infinity";
"???",
"&ang;",
"angle";
"???",
"&#8736;",
"angle";
"???",
"&and;",
"and";
"???",
"&#8743;",
"and";
"???",
"&or;",
"or";
"???",
"&#8744;",
"or";
"???",
"&cap;",
"cap";
"???",
"&#8745;",
"cap";
"???",
"&cup;",
"cup";
"???",
"&#8746;",
"cup";
"???",
"&int;",
"integral";
"???",
"&#8747;",
"integral";
"???",
"&there4;",
"therefore";
"???",
"&#8756;",
"therefore";
"???",
"&sim;",
"similar to";
"???",
"&#8764;",
"similar to";
"???",
"&cong;",
"congruent to";
"???",
"&#8773;",
"congruent to";
"???",
"&asymp;",
"almost equal";
"???",
"&#8776;",
"almost equal";
"???",
"&ne;",
"not equal";
"???",
"&#8800;",
"not equal";
"???",
"&equiv;",
"equivalent";
"???",
"&#8801;",
"equivalent";
"???",
"&le;",
"less or equal";
"???",
"&#8804;",
"less or equal";
"???",
"&ge;",
"greater or equal";
"???",
"&#8805;",
"greater or equal";
"???",
"&sub;",
"subset of";
"???",
"&#8834;",
"subset of";
"???",
"&sup;",
"superset of";
"???",
"&#8835;",
"superset of";
"???",
"&nsub;",
"not subset of";
"???",
"&#8836;",
"not subset of";
"???",
"&sube;",
"subset or equal";
"???",
"&#8838;",
"subset or equal";
"???",
"&supe;",
"superset or equal";
"???",
"&#8839;",
"superset or equal";
"???",
"&oplus;",
"circled plus";
"???",
"&#8853;",
"circled plus";
"???",
"&otimes;",
"circled times";
"???",
"&#8855;",
"circled times";
"???",
"&perp;",
"perpendicular";
"???",
"&#8869;",
"perpendicular";
"???",
"&sdot;",
"dot operator";
"???",
"&#8901;",
"dot operator";
"??",
"&Alpha;",
"Alpha";
"??",
"&#913;",
"Alpha";
"??",
"&Beta;",
"Beta";
"??",
"&#914;",
"Beta";
"??",
"&Gamma;",
"Gamma";
"??",
"&#915;",
"Gamma";
"??",
"&Delta;",
"Delta";
"??",
"&#916;",
"Delta";
"??",
"&Epsilon;",
"Epsilon";
"??",
"&#917;",
"Epsilon";
"??",
"&Zeta;",
"Zeta";
"??",
"&#918;",
"Zeta";
"??",
"&Eta;",
"Eta";
"??",
"&#919;",
"Eta";
"??",
"&Theta;",
"Theta";
"??",
"&#920;",
"Theta";
"??",
"&Iota;",
"Iota";
"??",
"&#921;",
"Iota";
"??",
"&Kappa;",
"Kappa";
"??",
"&#922;",
"Kappa";
"??",
"&Lambda;",
"Lambda";
"??",
"&#923;",
"Lambda";
"??",
"&Mu;",
"Mu";
"??",
"&#924;",
"Mu";
"??",
"&Nu;",
"Nu";
"??",
"&#925;",
"Nu";
"??",
"&Xi;",
"Xi";
"??",
"&#926;",
"Xi";
"??",
"&Omicron;",
"Omicron";
"??",
"&#927;",
"Omicron";
"??",
"&Pi;",
"Pi";
"??",
"&#928;",
"Pi";
"??",
"&Rho;",
"Rho";
"??",
"&#929;",
"Rho";
"??",
"&Sigma;",
"Sigma";
"??",
"&#931;",
"Sigma";
"??",
"&Tau;",
"Tau";
"??",
"&#932;",
"Tau";
"??",
"&Upsilon;",
"Upsilon";
"??",
"&#933;",
"Upsilon";
"??",
"&Phi;",
"Phi";
"??",
"&#934;",
"Phi";
"??",
"&Chi;",
"Chi";
"??",
"&#935;",
"Chi";
"??",
"&Psi;",
"Psi";
"??",
"&#936;",
"Psi";
"??",
"&Omega;",
"Omega";
"??",
"&#937;",
"Omega";
"??",
"&alpha;",
"alpha";
"??",
"&#945;",
"alpha";
"??",
"&beta;",
"beta";
"??",
"&#946;",
"beta";
"??",
"&gamma;",
"gamma";
"??",
"&#947;",
"gamma";
"??",
"&delta;",
"delta";
"??",
"&#948;",
"delta";
"??",
"&epsilon;",
"epsilon";
"??",
"&#949;",
"epsilon";
"??",
"&zeta;",
"zeta";
"??",
"&#950;",
"zeta";
"??",
"&eta;",
"eta";
"??",
"&#951;",
"eta";
"??",
"&theta;",
"theta";
"??",
"&#952;",
"theta";
"??",
"&iota;",
"iota";
"??",
"&#953;",
"iota";
"??",
"&kappa;",
"kappa";
"??",
"&#954;",
"kappa";
"??",
"&lambda;",
"lambda";
"??",
"&#955;",
"lambda";
"??",
"&mu;",
"mu";
"??",
"&#956;",
"mu";
"??",
"&nu;",
"nu";
"??",
"&#957;",
"nu";
"??",
"&xi;",
"xi";
"??",
"&#958;",
"xi";
"??",
"&omicron;",
"omicron";
"??",
"&#959;",
"omicron";
"??",
"&pi;",
"pi";
"??",
"&#960;",
"pi";
"??",
"&rho;",
"rho";
"??",
"&#961;",
"rho";
"??",
"&sigmaf;",
"sigmaf";
"??",
"&#962;",
"sigmaf";
"??",
"&sigma;",
"sigma";
"??",
"&#963;",
"sigma";
"??",
"&tau;",
"tau";
"??",
"&#964;",
"tau";
"??",
"&upsilon;",
"upsilon";
"??",
"&#965;",
"upsilon";
"??",
"&phi;",
"phi";
"??",
"&#966;",
"phi";
"??",
"&chi;",
"chi";
"??",
"&#967;",
"chi";
"??",
"&psi;",
"psi";
"??",
"&#968;",
"psi";
"??",
"&omega;",
"omega";
"??",
"&#969;",
"omega";
"??",
"&thetasym;",
"theta symbol";
"??",
"&#977;",
"theta symbol";
"??",
"&upsih;",
"upsilon symbol";
"??",
"&#978;",
"upsilon symbol";
"??",
"&piv;",
"pi symbol";
"??",
"&#982;",
"pi symbol";
"??",
"&OElig;",
"capital ligature OE";
"??",
"&#338;",
"capital ligature OE";
"??",
"&oelig;",
"small ligature oe";
"??",
"&#339;",
"small ligature oe";
"??",
"&Scaron;",
"capital S with caron";
"??",
"&#352;",
"capital S with caron";
"??",
"&scaron;",
"small S with caron";
"??",
"&#353;",
"small S with caron";
"??",
"&Yuml;",
"capital Y with diaeres";
"??",
"&#376;",
"capital Y with diaeres";
"??",
"&fnof;",
"f with hook";
"??",
"&#402;",
"f with hook";
"??",
"&circ;",
"modifier letter circumflex accent";
"??",
"&#710;",
"modifier letter circumflex accent";
"??",
"&tilde;",
"small tilde";
"??",
"&#732;",
"small tilde";
"???",
"&ensp;",
"en space";
"???",
"&#8194;",
"en space";
"???",
"&emsp;",
"em space";
"???",
"&#8195;",
"em space";
"???",
"&thinsp;",
"thin space";
"???",
"&#8201;",
"thin space";
"???",
"&zwnj;",
"zero width non-joiner";
"???",
"&#8204;",
"zero width non-joiner";
"???",
"&zwj;",
"zero width joiner";
"???",
"&#8205;",
"zero width joiner";
"???",
"&lrm;",
"left-to-right mark";
"???",
"&#8206;",
"left-to-right mark";
"???",
"&rlm;",
"right-to-left mark";
"???",
"&#8207;",
"right-to-left mark";
"???",
"&ndash;",
"en dash";
"???",
"&#8211;",
"en dash";
"???",
"&mdash;",
"em dash";
"???",
"&#8212;",
"em dash";
"???",
"&lsquo;",
"left single quotation mark";
"???",
"&#8216;",
"left single quotation mark";
"???",
"&rsquo;",
"right single quotation mark";
"???",
"&#8217;",
"right single quotation mark";
"???",
"&sbquo;",
"single low-9 quotation mark";
"???",
"&#8218;",
"single low-9 quotation mark";
"???",
"&ldquo;",
"left double quotation mark";
"???",
"&#8220;",
"left double quotation mark";
"???",
"&rdquo;",
"right double quotation mark";
"???",
"&#8221;",
"right double quotation mark";
"???",
"&bdquo;",
"double low-9 quotation mark";
"???",
"&#8222;",
"double low-9 quotation mark";
"???",
"&dagger;",
"dagger";
"???",
"&#8224;",
"dagger";
"???",
"&Dagger;",
"double dagger";
"???",
"&#8225;",
"double dagger";
"???",
"&bull;",
"bullet";
"???",
"&#8226;",
"bullet";
"???",
"&hellip;",
"horizontal ellipsis";
"???",
"&#8230;",
"horizontal ellipsis";
"???",
"&permil;",
"per mille&nbsp;";
"???",
"&#8240;",
"per mille&nbsp;";
"???",
"&prime;",
"minutes";
"???",
"&#8242;",
"minutes";
"???",
"&Prime;",
"seconds";
"???",
"&#8243;",
"seconds";
"???",
"&lsaquo;",
"single left angle quotation";
"???",
"&#8249;",
"single left angle quotation";
"???",
"&rsaquo;",
"single right angle quotation";
"???",
"&#8250;",
"single right angle quotation";
"???",
"&oline;",
"overline";
"???",
"&#8254;",
"overline";
"???",
"&euro;",
"euro";
"???",
"&#8364;",
"euro";
"???",
"&trade;",
"trademark";
"???",
"&#8482;",
"trademark";
"???",
"&trade;",
"trademark";
"???",
"&#153;",
"trademark";
"???",
"&larr;",
"left arrow";
"???",
"&#8592;",
"left arrow";
"???",
"&uarr;",
"up arrow";
"???",
"&#8593;",
"up arrow";
"???",
"&rarr;",
"right arrow";
"???",
"&#8594;",
"right arrow";
"???",
"&darr;",
"down arrow";
"???",
"&#8595;",
"down arrow";
"???",
"&harr;",
"left right arrow";
"???",
"&#8596;",
"left right arrow";
"???",
"&crarr;",
"carriage return arrow";
"???",
"&#8629;",
"carriage return arrow";
"???",
"&lceil;",
"left ceiling";
"???",
"&#8968;",
"left ceiling";
"???",
"&rceil;",
"right ceiling";
"???",
"&#8969;",
"right ceiling";
"???",
"&lfloor;",
"left floor";
"???",
"&#8970;",
"left floor";
"???",
"&rfloor;",
"right floor";
"???",
"&#8971;",
"right floor";
"???",
"&loz;",
"lozenge";
"???",
"&#9674;",
"lozenge";
"???",
"&spades;",
"spade";
"???",
"&#9824;",
"spade";
"???",
"&clubs;",
"club";
"???",
"&#9827;",
"club";
"???",
"&hearts;",
"heart";
"???",
"&#9829;",
"heart";
"???",
"&diams;",
"diamond";
"???",
"&#9830;",
"diamond";
]
