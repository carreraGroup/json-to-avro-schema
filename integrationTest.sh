#! /bin/bash
echo "creating output directory"
mkdir -p output
for f in src/test/resources/integration-tests/*.json; do
    input_name=$(basename $f)
    output_name=${input_name%.json}.avsc
    echo
    echo "converting $input_name to $output_name"
    java -jar target/scala-2.13/json-to-avro-schema-assembly-0.1.jar $f > output/$output_name
done

if [ ! -f avro-tools-1.10.1.jar ]; then
    echo "avro-tools not found"
    echo "hang on, we're downloading it..."
    wget https://mirror.jframeworks.com/apache/avro/avro-1.10.1/java/avro-tools-1.10.1.jar
fi

echo
echo "generating java from generated avsc files"
java -jar avro-tools-1.10.1.jar compile schema output/*.avsc output/