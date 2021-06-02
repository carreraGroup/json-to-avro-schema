#! /bin/bash
echo "creating output directory"
mkdir -p output
for f in src/test/resources/integration-tests/*.json; do
    input_name=$(basename $f)
    output_name=${input_name%.json}.avsc
    echo
    echo "converting $input_name to $output_name"
    java -jar target/scala-2.13/json-to-avro-schema-assembly-0.1.jar $f output
done

avro_tools_version='1.10.2'
avro_tools="avro-tools-$avro_tools_version.jar"

if [ ! -f $avro_tools ]; then
    echo "avro-tools not found"
    echo "hang on, we're downloading it..."
    wget https://ftp.wayne.edu/apache/avro/avro-$avro_tools_version/java/$avro_tools
fi

echo
echo "generating java from generated avsc files"
java -jar $avro_tools compile schema output/*.avsc output/