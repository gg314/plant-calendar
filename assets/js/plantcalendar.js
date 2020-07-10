
function plantCalendar(json) {
    var doc = new jsPDF({
    unit: 'in',
    format: 'letter'
    });

    var location = json["location"];
    var veggies = json["plants"];
    var springFrost = json["sf"];
    var winterFrost = json["wf"];
    var springFrostString = json["sfstr"];
    var winterFrostString = json["wfstr"];

    var igs = new doc.GState({ "opacity": 0.2 });
    var tgs = new doc.GState({ "opacity": 0.7 });
    var ogs = new doc.GState({ "opacity": 1 });
        
    var lm = 0.5;
    var rm = 8.0;
    var sm = 2.5; // start margin
    var sh = 3.075;
    var eh = 10.5;
    var inc = (rm-sm)/12.0;
    var vinc = 0.5;

    doc.setDrawColor(128, 128, 128);
    doc.setFont("helvetica");
    doc.setFontSize(8);
    doc.text("https://gg314.github.io/plant-calendar/", 7.875, 0.75, null, null, "right");

    doc.setDrawColor(68, 68, 68);
    doc.setFont("helvetica");
    doc.setFontSize(14);
    doc.text("Location:", 0.625, 0.75);
    doc.text("Average last spring frost:", 0.625, 1.10);
    doc.text("Average first winter frost:", 0.625, 1.45);

    doc.setFontStyle("bold");
    doc.text(location, 1.475, 0.75);
    doc.text(springFrostString, 2.83, 1.10);
    doc.text(winterFrostString, 2.82, 1.45);
    doc.setFontStyle("normal");


    doc.setLineDash([0.02]);
    doc.setDrawColor(188, 188, 188);
    doc.setLineWidth(0.006);
    doc.line(lm, 1.75, rm, 1.75);

    doc.setTextColor(68, 68, 68);
    doc.setFontStyle("normal");


    function dividers(y1, y2) {
        doc.setGState(igs);
        doc.setLineDash();
        doc.setDrawColor(0, 0, 0);
        doc.setFillColor(0, 0, 0);
        for(i = 0; i <= 12; i++) {
            doc.rect(sm+i*inc-.003, y1, .006, y2-y1, "F");
        }
        doc.setGState(ogs);
    }
    
    function drawFrost(x, y1, y2) {
        var x0 = x/365*(rm-sm) + sm;
        console.log(springFrost, x0);
        doc.setGState(tgs);
        doc.setLineDash([0.05]);
        doc.setDrawColor(173, 191, 211);
        doc.setLineWidth(0.02);
        doc.line(x0, y1, x0, y2);
        // doc.rect(x0-.01, y1, .02, y2-y1, "F");
        doc.setGState(ogs);
    }

    function dloop(h, t) {
        doc.setDrawColor(0);
        doc.setLineDash();
        doc.setLineWidth(0.006);
        doc.line(lm, h, rm, h);
        for(i = 0; i <= 12; i++) {
            doc.line(sm+i*inc, h-0.125, sm+i*inc, h+0.125);
        }
        doc.setTextColor(68, 68, 68);
        doc.setFontStyle("normal");
        doc.setFont("GloriaHallelujah-Regular");
        doc.setFontSize(13);
        doc.text(t, lm, h-.05, null, null, "left");
        
    }

    function drawCycle(h, type, start, duration) {

        doc.setGState(tgs)
        
        doc.setLineDash();
        doc.setLineWidth(0.006);

        start = start/365*(rm-sm) + sm;
        duration = duration/365*(rm-sm);
        switch(type) {
            case "i":
                doc.setDrawColor(119, 115, 79);
                doc.setFillColor(209, 193, 42);
                doc.rect(start, h-0.075, duration, 0.15, "FD"); // Y
                break;

            case "t":
                doc.setDrawColor(81, 127, 108);
                doc.setFillColor(56, 165, 116);
                doc.rect(start, h-0.075, duration, 0.15, "FD"); // G
                break;

            case "o":
                doc.setDrawColor(124, 102, 80);
                doc.setFillColor(211, 122, 41);
                doc.rect(start, h-0.075, duration, 0.15, "FD"); // O
                break;
        }
        doc.setGState(ogs)

    }


    function drawLegend(h) {

        var tgas = new doc.GState({ "opacity": 0.7, "stroke-opacity": 1.0 });
        doc.setGState(tgas);

        doc.setFont("helvetica");
        doc.setFontStyle("normal");
        doc.setFontSize(12);
        doc.setLineDash();
        doc.setLineWidth(0.006);
        doc.setDrawColor(119, 115, 79);
        doc.setFillColor(209, 193, 42);
        doc.rect(1.0, h-0.35, 0.5, 0.15, "FD");
        doc.text("Plant indoors", 1.25, h, null, null, "center");

        doc.setDrawColor(81, 127, 108);
        doc.setFillColor(56, 165, 116);
        doc.rect(3.0, h-.35, 0.5, 0.15, "FD");
        doc.text("Transplant seedlings", 3.25, h, null, null, "center");

        doc.setDrawColor(124, 102, 80);
        doc.setFillColor(211, 122, 41);
        doc.rect(5.0, h-.35, 0.5, 0.15, "FD");
        doc.text("Plant outdoors", 5.25, h, null, null, "center");

        doc.setLineDash([0.05]);
        doc.setDrawColor(173, 191, 211);
        doc.setLineWidth(0.02);
        doc.line(7.0, h-.275, 7.5, h-.275);
        doc.text("Average frost date", 7.25, h, null, null, "center");

        doc.setGState(ogs)
        
        doc.setDrawColor(0);
        doc.setFontSize(10);
        doc.setFontStyle("bold");
        doc.setTextColor(189, 194, 190);
        doc.text("JAN", sm+0*inc, h+0.5, null, null, "center");
        doc.text("FEB", sm+1*inc, h+0.5, null, null, "center");
        doc.text("MAR", sm+2*inc, h+0.5, null, null, "center");
        doc.text("APR", sm+3*inc, h+0.5, null, null, "center");
        doc.text("MAY", sm+4*inc, h+0.5, null, null, "center");
        doc.text("JUN", sm+5*inc, h+0.5, null, null, "center");
        doc.text("JUL", sm+6*inc, h+0.5, null, null, "center");
        doc.text("AUG", sm+7*inc, h+0.5, null, null, "center");
        doc.text("SEP", sm+8*inc, h+0.5, null, null, "center");
        doc.text("OCT", sm+9*inc, h+0.5, null, null, "center");
        doc.text("NOV", sm+10*inc, h+0.5, null, null, "center");
        doc.text("DEC", sm+11*inc, h+0.5, null, null, "center");

    }

    drawLegend(2.4);

    doc.setDrawColor(150, 150, 150);
    doc.setLineWidth(0.006);

    var h = 2.9;
    var vertStart = 3.075;
    var vertEnd = 0;

    for(v = 0; v < veggies.length; v++) {
        if (h > 9.75) {
            doc.addPage('letter');
            drawLegend(1.0);
            h = 2.0;
            vertStart = h - .325;
        } else {
            h += 0.5;
            vertEnd = h + .25;
        }
        dloop(h, veggies[v]["name"]);
        for (s = 0; s < veggies[v]["boxes"].length; s++) {
            var box = veggies[v]["boxes"][s];
            drawCycle(h, box["type"], box["start"], box["duration"]);
        }
        
        if (h > 9.75 || v == (veggies.length-1)) {
        dividers(vertStart, vertEnd);
        drawFrost(springFrost, vertStart, vertEnd);
        drawFrost(winterFrost, vertStart, vertEnd);
        }
    }

    doc.save('plant-calendar.pdf')
}


function testCalendar(arg) {
    console.log("Saving PDF with arguments:");
    console.log(arg);
}