xquery version "3.0";

declare namespace html = "http://www.w3.org/1999/xhtml";
declare variable $ko-kr external;
declare variable $ko-kp external;
declare variable $ko-kore external;

replace node //html:div/html:p[@id="placeholder-ko-kr"]
with doc($ko-kr),
replace node //html:div/html:p[@id="placeholder-ko-kp"]
with doc($ko-kp),
replace node //html:div/html:p[@id="placeholder-ko-kore"]
with doc($ko-kore)
