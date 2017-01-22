#!/usr/bin/python

# adds margins around a pdf.  
# pdf_margins.py /path/to/file.pdf N
# N is optional.  it defines the size of the margins as percent of the page.  defaults to 10.
# output is file_cropped.pdf

from pyPdf import PdfFileWriter, PdfFileReader
from sys   import argv
from re    import sub

marginsize = 10 # sane default.  adds enough margin to marginless pages (from D&D 4e char builder)

# no args, print usage
if len(argv) == 1:
    print "\nusage: ./pdf-margins.py /path/to/file.pdf margin_size\nA pdf file is mandatory.  Margin size is not.  Margin size is the percentage of the page the margins will take up.  It defaults to 5.\n"
    exit(1)

# first arg is input file
if len(argv) > 1:
    inFile = argv[1]
    outFile = sub( "\.pdf$", "_cropped.pdf", inFile )
    print outFile

# second arg is size of margins, in percent of page
if len(argv) > 2:
    print type(argv[2])
    if 0 <= int(argv[2]) <= 50:
        marginsize = int(argv[2])
    else:
        print "\nMargin is a percentage.  It must be a number between 0 and 50."
        exit(1)


# output and input files
output = PdfFileWriter()
input = PdfFileReader(file(inFile,"rb"))

# read all pages.  change media and crop boxes.  they both get pushed out by marginsize percent
for n in range(0, input.getNumPages()):
    page = input.getPage(n)

    page.mediaBox.upperRight = (
        (page.mediaBox.getUpperRight_x() * (100+marginsize)) / 100,
        (page.mediaBox.getUpperRight_y() * (100+marginsize)) / 100,
        )
    page.mediaBox.lowerLeft = (
        page.mediaBox.getLowerLeft_x() - page.mediaBox.getUpperRight_x()*marginsize/100,
        page.mediaBox.getLowerLeft_y() - page.mediaBox.getUpperRight_y()*marginsize/100,
        )

    page.cropBox = page.mediaBox
    page.trimBox = page.mediaBox

    output.addPage(page)

# write to output file
fh = file(outFile, "wb")
output.write(fh)
fh.close()
