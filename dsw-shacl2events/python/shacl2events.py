import click
import json
import rdflib
import re
import time
import uuid

SHACL = rdflib.namespace.Namespace('http://www.w3.org/ns/shacl#')
RDF = rdflib.RDF
XSD = rdflib.XSD


class UUIDGenerator:

    def __init__(self):
        self.uuids = set()

    def generate(self):
        result = uuid.uuid4()
        while result in self.uuids:
            result = uuid.uuid4()
        self.uuids.add(result)
        return str(result)


class Question:

    def __init__(self, uuid, parent_uuid, title, type, required_level=None, text=None):
        self.uuid = uuid
        self.title = title
        self.text = text
        self.type = type
        self.required_level = required_level
        self.parent_uuid = parent_uuid

    @property
    def add_event(self):
        return {
            'uuid': None,
            'parentUuid': self.parent_uuid,
            'entityUuid': self.uuid,
            'eventType': 'AddQuestionEvent',
            'title': self.title,
            'questionType': self.type,
            'text': self.text,
            'requiredLevel': self.required_level,
            'tagUuids': []
        }


class ValueQuestion(Question):
    TYPE = 'ValueQuestion'
    STRING_TYPE = 'StringQuestionValueType'
    TEXT_TYPE = 'TextQuestionValueType'
    DATE_TYPE = 'DateQuestionValueType'
    NUMBER_TYPE = 'NumberQuestionValueType'

    def __init__(self, uuid, parent_uuid, title, value_type, required_level=None, text=None):
        super().__init__(uuid, parent_uuid, title, self.TYPE, required_level, text)
        self.value_type = value_type

    @property
    def add_event(self):
        event = super().add_event
        event['valueType'] = self.value_type
        return event


class ListQuestion(Question):
    TYPE = 'ListQuestion'

    def __init__(self, uuid, parent_uuid, title, required_level=None, text=None):
        super().__init__(uuid, parent_uuid, title, self.TYPE, required_level, text)


class OptionsQuestion(Question):
    TYPE = 'OptionsQuestion'

    def __init__(self, uuid, parent_uuid, title, required_level=None, text=None):
        super().__init__(uuid, parent_uuid, title, self.TYPE, required_level, text)


class Answer:

    def __init__(self, uuid, parent_uuid, label, advice=None):
        self.uuid = uuid
        self.parent_uuid = parent_uuid
        self.label = label
        self.advice = advice

    @property
    def add_event(self):
        return {
            'advice': self.advice,
            'uuid': None,
            'entityUuid': self.uuid,
            'parentUuid': self.parent_uuid,
            'eventType': 'AddAnswerEvent',
            'metricMeasures': [],
            'label': self.label
        }


class Chapter:

    def __init__(self, uuid, km_uuid, title, text=None):
        self.uuid = uuid
        self.title = title
        self.text = text
        self.km_uuid = km_uuid

    @property
    def add_event(self):
        return {
            'uuid': None,
            'parentUuid': self.km_uuid,
            'entityUuid': self.uuid,
            'eventType': 'AddChapterEvent',
            'title': self.title,
            'text': self.text
        }


class URLReference:

    def __init__(self, uuid, question_uuid, url, label=None):
        self.uuid = uuid
        self.question_uuid = question_uuid
        self.url = url
        self.label = label or url

    @property
    def add_event(self):
        return {
            'uuid': None,
            'url': self.url,
            'entityUuid': self.uuid,
            'parentUuid': self.question_uuid,
            'eventType': 'AddReferenceEvent',
            'referenceType': 'URLReference',
            'label': self.label
        }


class KnowledgeModelPackage:

    def __init__(self, uuid, name, identifier, org, version):
        self.uuid = uuid
        self.name = name
        self.identifier = identifier
        self.org = org
        self.version = version
        self.events = [self.add_event]

    @property
    def add_event(self):
        return {
            'uuid': None,
            'entityUuid': self.uuid,
            'parentUuid': '00000000-0000-0000-0000-000000000000',
            'eventType': 'AddKnowledgeModelEvent',
            'name': self.name
        }

    def _prepare_events(self):
        gen = UUIDGenerator()
        for event in self.events:
            event['uuid'] = gen.generate()
        return self.events

    @property
    def package(self):
        return {
            'readme': '# '+self.name,
            'createdAt': time.strftime('%Y-%m-%dT%H:%M:%SZ'),
            'metamodelVersion': 4,
            'kmId': self.identifier,
            'forkOfPackageId': None,
            'name': self.name,
            'version': self.version,
            'id': {self.org}+':'+{self.identifier}+':'+{self.version},
            'mergeCheckpointPackageId': None,
            'license': 'nolicense',
            'organizationId': self.org,
            'previousPackageId': None,
            'description': 'Knowledge Model generated from SHACL',
            'events': self._prepare_events()
        }

    @property
    def package_bundle(self):
        return {
            'metamodelVersion': 4,
            'kmId': self.identifier,
            'name': self.name,
            'version': self.version,
            'id': {self.org}+':'+{self.identifier}+':'+{self.version},
            'organizationId': self.org,
            'packages': [
                self.package
            ]
        }


class KMBuilder:
    EXCLUDED_CHAPTERS = ['Metadata', 'Publisher']

    def __init__(self, name='KM from SHACL', identifier='km', version='1.0.0', organization='dsw'):
        self.gen = UUIDGenerator()
        self.km = KnowledgeModelPackage(self.gen.generate(), name, identifier, organization, version)
        self.ref_lookup = dict()

    def _add_chapter(self, title, text):
        chapter = Chapter(self.gen.generate(), self.km.uuid, title, text)
        self.km.events.append(chapter.add_event)
        return chapter

    def _add_value_question(self, parent_uuid, title, value_type, required_level, text):
        question = ValueQuestion(self.gen.generate(), parent_uuid, title, value_type, required_level, text)
        self.km.events.append(question.add_event)
        return question

    def _add_list_question(self, parent_uuid, title, required_level, text):
        question = ListQuestion(self.gen.generate(), parent_uuid, title, required_level, text)
        self.km.events.append(question.add_event)
        return question

    def _add_options_question(self, parent_uuid, title, required_level, text):
        question = OptionsQuestion(self.gen.generate(), parent_uuid, title, required_level, text)
        answer = Answer(self.gen.generate(), question.uuid, 'Fill')
        self.km.events.append(question.add_event)
        self.km.events.append(answer.add_event)
        return question, answer

    def _add_url_reference(self, url, question_uuid):
        ref = URLReference(self.gen.generate(), question_uuid, url)
        self.km.events.append(ref.add_event)
        return ref

    def add_question(self, parent_uuid, property_shape):
        if not SHACL['name'] in property_shape.meta:
            return
        # TODO: sh:or
        name = property_shape.meta.get(SHACL['name'])
        is_mandatory = int(property_shape.meta.get(SHACL['minCount'], 0)) == 1
        is_list = int(property_shape.meta.get(SHACL['maxCount'], 2)) > 1
        uri_path = property_shape.meta.get(SHACL['path'], None)
        datatype = property_shape.meta.get(SHACL['datatype'], None)
        description = property_shape.meta.get(SHACL['description'], '')

        value_type = ValueQuestion.STRING_TYPE
        if datatype == XSD['dateTime']:
            value_type = ValueQuestion.DATE_TYPE
        elif datatype == XSD['int']:
            value_type = ValueQuestion.NUMBER_TYPE

        required_level = 1 if is_mandatory else None

        q = None
        if len(property_shape.nodes) > 0:
            for node in property_shape.nodes:
                q, a = self._add_options_question(parent_uuid, name, required_level, description)
                node_shape = self.ref_lookup[node]
                for property_shape in node_shape.property_shapes:
                    self.add_question(a.uuid, property_shape)
        elif is_list:
            q = self._add_list_question(parent_uuid, name, required_level, description)
            self._add_value_question(q.uuid, name, value_type, required_level, description)
        else:
            q = self._add_value_question(parent_uuid, name, value_type, required_level, description)
        if q is not None:
            self._add_url_reference(uri_path, q.uuid)

    def add_node_shape_chapter(self, node_shape):
        target_class = node_shape.meta.get(SHACL['targetClass'], '')
        chapter_name = re.split('/|#', node_shape.identifier)[-1]
        if chapter_name in self.EXCLUDED_CHAPTERS:
            return
        chapter = self._add_chapter(chapter_name, 'According to ' + target_class)
        for property_shape in node_shape.property_shapes:
            self.add_question(chapter.uuid, property_shape)


class NodeShape:

    def __init__(self, identifier, **metadata):
        self.identifier = identifier
        self.meta = dict(metadata)
        self.property_shapes = []


class PropertyShape:

    def __init__(self, **metadata):
        self.meta = dict(metadata)
        self.nodes = []


def rdf_list(g, subject):
    result = []
    for first_obj in g.objects(subject, RDF['first']):
        result.append(first_obj)
    for rest_obj in g.objects(subject, RDF['rest']):
        if rest_obj == RDF['nil']:
            return result
        else:
            result.extend(rdf_list(g, rest_obj))
    return result


def parse_property_shape(g: rdflib.Graph, property_shape):
    ps = PropertyShape(**{
        p: o for p, o in g.predicate_objects(property_shape)
    })
    for node in g.objects(property_shape, SHACL['node']):
        ps.nodes.append(node)
    return ps


def parse_node_shape(g: rdflib.Graph, node_shape):
    ns = NodeShape(node_shape, **{
        p: o for p, o in g.predicate_objects(node_shape)
    })
    ns.property_shapes = get_property_shapes(g, node_shape)
    ns.property_shapes.sort(key=lambda p: p.meta.get(SHACL.order, 999))
    return ns


def get_property_shapes(g, node_shape):
    property_shapes = []

    for property_shape in g.objects(node_shape, SHACL.property):
        property_shapes.append(parse_property_shape(g, property_shape))

    for andjoin in g.objects(node_shape, SHACL['and']):
        l = rdf_list(g, andjoin)
        for entry in l:
            if (entry, RDF.type, SHACL.PropertyShape) in g:
                property_shapes.append(parse_property_shape(g, entry))
            if (entry, RDF.type, SHACL.NodeShape) in g:
                property_shapes.extend(get_property_shapes(g, entry))

    return property_shapes


@click.command()
@click.argument('shacls', metavar='SHACL', nargs=-1, type=click.File('r'))
def cli(shacls):
    """Tool to convert SHACL file(s) to DSW events"""
    g = rdflib.Graph()
    for shacl in shacls:
        g.parse(file=shacl, format="n3")

    kmb = KMBuilder()
    nodes = {}
    for node_shape in g.subjects(rdflib.RDF.type, SHACL.NodeShape):
        nodes[node_shape] = parse_node_shape(g, node_shape)
    kmb.ref_lookup = nodes
    for node_shape in nodes.values():
        kmb.add_node_shape_chapter(node_shape)
    print(json.dumps(kmb.km._prepare_events(), indent=4))


if __name__ == '__main__':
    cli()
