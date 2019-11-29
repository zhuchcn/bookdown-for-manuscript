from lxml import etree
import zipfile
import os
import shutil
import argparse


class Docx():
    def __init__(self, input_name, output_name):
        self.input_name = input_name
        self.output_name = output_name
        self.tempdir = "tempdir/"

        xml_content = zipfile.ZipFile(self.input_name).read('word/document.xml')
        self.tree = etree.fromstring(xml_content)
    
    def formatEquations(self, font):
        for node in self.tree.findall(".//{http://schemas.openxmlformats.org/officeDocument/2006/math}r"):
            el = etree.fromstring(b'<m:rPr xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas" xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:mv="urn:schemas-microsoft-com:mac:vml" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape"><m:nor/></m:rPr>')
            node.insert(0, el)
            for child in node.findall(".//{http://schemas.openxmlformats.org/wordprocessingml/2006/main}rFonts"):
                child.attrib['{http://schemas.openxmlformats.org/wordprocessingml/2006/main}ascii'] = font
                child.attrib['{http://schemas.openxmlformats.org/wordprocessingml/2006/main}hAnsi'] = font
        print("The font of equations was changed", flush=True)
        return self

    def save(self):
        zip = zipfile.ZipFile(self.input_name)
        zip.extractall(self.tempdir)

        with open(os.path.join(self.tempdir, "word/document.xml"), "w") as f:
            xmlstr = etree.tostring(self.tree, encoding = "unicode", method = "xml")
            f.write(xmlstr)

        with zipfile.ZipFile(self.output_name, "w") as myzip:
            for filename in zip.namelist():
                myzip.write(os.path.join(self.tempdir, filename), filename)
        
        shutil.rmtree(self.tempdir)
        print("The docx file was saved", flush=True)
        return

if __name__ == "__main__":
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("-i", "--input-file", type=str, default=None,
                            help="Input file path")
    arg_parser.add_argument("-o", "--output-file", type=str, default=None,
                            help="Output file path")
    arg_parser.add_argument("-f", "--font", type=str, default=None,
                            help="The font that the equations to be changed to.")
    args = arg_parser.parse_args()
    Docx(args.input_file, args.output_file).formatEquations(args.font).save()
