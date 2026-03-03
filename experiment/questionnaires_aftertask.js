const questionnaire_instructions = {
    type: jsPsychSurvey,
    survey_json: {
        showQuestionNumbers: false,
        completeText: "Let's start",
        pages: [
            {
                elements: [
                    {
                        type: "html",
                        name: "Instructions",
                        html:
                            "<div style='display: flex;'>" +
                            "<div style='width: 60%; margin-right: 20px;'>" +
                            "<h2>Instructions</h2>" +
                            "<p>You will now be asked to complete various questionnaires. Please read each question carefully, and don't hesitate to take breaks in-between them.</p>" +
                            "<p>Please note that <b style='color:#FF5722;'>various checks will be performed to ensure the validity of the data</b>.</li>" +
                            "<p> We are aware that answering these questionnaires <b>might feel long and repetitive</b>, but having similar questions is necessary to ensure the validity of the results (we expect similar questions to be related, and opposite questions to be inversely related). " +
                            "</div>" +
                            "<div style='width: 40%;'>" +
                            "<img src='https://www.simplypsychology.org/wp-content/uploads/psychology.jpeg' alt='Illustration' style='width: 100%;'>" +
                            "</div>" +
                            "</div>",
                    },
                ],
            },
        ],
    },
}

// MINT Items ================================================
const items_mint = {
    MINT_ExAc_1: "I can always accurately feel when I am about to fart",
    MINT_ExAc_2: "I can always accurately feel when I am about to sneeze",
    MINT_ExAc_3: "I can always accurately feel when I am about to burp",
    MINT_RelA_4: "I always feel in my body if I am relaxed",
    MINT_RelA_5: "I always know when I am relaxed",
    MINT_RelA_6: "My body is always in the same specific state when I am relaxed",
    MINT_SexS_7: "During sex or masturbation, I often feel very strong sensations coming from my genital areas",
    MINT_SexS_8: "My genital organs are very sensitive to pleasant stimulations",
    MINT_SexS_9: "When I am sexually aroused, I often notice specific sensations in my genital area (e.g., tingling, warmth, wetness, stiffness, pulsations)",
    MINT_CaCo_10: "Sometimes my breathing becomes erratic or shallow and I often don't know why",
    MINT_CaCo_11: "I often feel like I can't get enough oxygen by breathing normally",
    MINT_CaCo_12: "Sometimes my heart starts racing and I often don't know why",
    MINT_Urin_13: "I sometimes feel like I need to urinate or defecate but when I go to the bathroom I produce less than I expected",
    MINT_Urin_14: "I often feel the need to urinate even when my bladder is not full",
    MINT_Urin_15: "Sometimes I am not sure whether I need to go to the toilet or not (to urinate or defecate)",
    MINT_Derm_16: "In general, my skin is very sensitive",
    MINT_Derm_17: "My skin is susceptible to itchy fabrics and materials",
    MINT_Derm_18: "I can notice even very subtle stimulations to my skin (e.g., very light touches)",
    MINT_Sati_19: "I don't always feel the need to eat until I am really hungry",
    MINT_Sati_20: "Sometimes I don't realise I was hungry until I ate something ",
    MINT_Sati_21: "I don't always feel the need to drink until I am really thirsty",
    MINT_Olfa_22: "I often check the smell of my armpits",
    MINT_Olfa_23: "I often check the smell of my own breath",
    MINT_Olfa_24: "I often check the smell of my farts",
    MINT_Resp_25: "In general, I am very sensitive to changes in my breathing",
    MINT_Resp_26: "I can notice even very subtle changes in my breathing",
    MINT_Resp_27: "I am always very aware of how I am breathing, even when I am calm",
    MINT_Card_28: "In general, I am very sensitive to changes in my heart rate",
    MINT_Card_29: "I often notice changes in my heart rate",
    MINT_Card_30: "I can notice even very subtle changes in the way my heart beats",
    MINT_Gast_31: "I can notice even very subtle changes in what my stomach is doing",
    MINT_Gast_32: "In general, I am very sensitive to what my stomach is doing",
    MINT_Gast_33: "I am always very aware of what my stomach is doing, even when I am calm",
    MINT_AttentionCheck: "I can always accurately answer to the extreme left on this question to show that I am reading it",
}

// Generation code ================================================
// Convernience function to shuffle an object (used internally)
function shuffleObject(obj) {
    const entries = Object.entries(obj)
    for (let i = entries.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1))
        ;[entries[i], entries[j]] = [entries[j], entries[i]]
    }
    return Object.fromEntries(entries)
}

// This function formats each question into a jsPsych-survey question that contains information about the question format
function make_mint(items, required = true, ticks = ["Disagree", "Agree"]) {
    items = shuffleObject(items)

    questions = []

    // Make questions
    for (const key of Object.keys(items)) {
        q = {
            title: items[key],
            name: key,
            type: "rating",
            displayMode: "buttons",
            // scaleColorMode: "colored",
            isRequired: required,
            minRateDescription: ticks[0],
            maxRateDescription: ticks[1],
            rateValues: [0, 1, 2, 3, 4, 5, 6],
        }
        questions.push(q)
    }

    return { elements: questions }
}

// MINT Questionnaire ================================================
const questionnaire_mint = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "About you and your body",
            description:
                "Please answer the following questions based on how accurately each statement describes you in general.",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: make_mint(items_mint),
        }
    },
    data: {
        screen: "questionnaire_mint",
    },
}


// BAIT ================================================

const items_bait = {
    BAIT_ImagesRealistic_1: "Current AI algorithms can generate very realistic images",
    BAIT_ImagesIssues_2: "Images of faces or people generated by AI always contain errors and artifacts",
    BAIT_VideosRealistic_3: "Videos generated by AI have obvious problems that make them easy to spot as fake",
    BAIT_VideosIssues_4: "Current AI algorithms can generate very realistic videos",
    BAIT_ImitatingReality_5: "Computer-Generated Images (CGI) are capable of perfectly imitating reality",
    BAIT_EnvironmentReal_6: "Technology allows the creation of environments that seem just as real as reality",
    BAIT_TextRealistic_7: "AI assistants can write texts that are indistinguishable from those written by humans",
    BAIT_TextIssues_8: "Documents and paragraphs written by AI usually read differently compared to Human productions",
    BAIT_NegativeAttitutes_9: "AI is dangerous",
    BAIT_NegativeAttitutes_10: "I am worried about future uses of AI",
    BAIT_PositiveAttitutes_11: "AI is exciting",
    BAIT_PositiveAttitutes_12: "Much of society will benefit from a future full of AI",
    BAIT_AttentionCheck: "I can show that I am Human and not an AI by answering all the way to the right",
}

function make_bait(items, required = true, ticks = ["Disagree", "Agree"]) {
    items = shuffleObject(items)
    var ai_expertise = [
        {
            title: "How knowledgeable do you consider yourself about Artificial Intelligence (AI) technology?",
            name: "BAIT_AI_Knowledge",
            type: "rating",
            displayMode: "buttons",
            isRequired: required,
            minRateDescription: "Not at all",
            maxRateDescription: "Expert",
            rateValues: [0, 1, 2, 3, 4, 5, 6],
        },
        {
            title: "How frequently do you use Artificial Intelligence (AI) tools or technologies in your daily life? ",
            description: "This includes tools like ChatGPT, image or art generators, and AI assistants.",
            name: "BAIT_AI_Use",
            type: "radiogroup",
            isRequired: required,
            choices: ["Never", "A few times per month", "A few times per week", "Once a day", "A few times per day"],
        },
    ]

    questions = []

    // Make questions
    for (const key of Object.keys(items)) {
        q = {
            title: items[key],
            name: key,
            type: "rating",
            displayMode: "buttons",
            // scaleColorMode: "colored",
            isRequired: required,
            minRateDescription: ticks[0],
            maxRateDescription: ticks[1],
            rateValues: [0, 1, 2, 3, 4, 5, 6],
        }
        questions.push(q)
    }
    return [
        { elements: ai_expertise },
        {
            elements: questions,
            description:
                "We are interested in your thoughts about Artificial Intelligence (AI). Please read the statements below carefully and indicate the extent to which you agree with each statement.",
        },
    ]
}

const questionnaire_bait = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "About Artificial Intelligence (AI)",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: make_bait(items_bait),
        }
    },
    data: {
        screen: "questionnaire_bait",
    },
}


// IRI ================================================

const items_iri = {
    IRI_Cogntive_1: "I try to look at everybody's side of a disagreement before I make a decision.",
    IRI_Cogntive_2: "I sometimes try to understand my friends better by imagining how things look from their perspective.",
    IRI_Cogntive_3: "I believe that there are two sides to every question and try to look at them both.",
    IRI_Cognitive_4: "I sometimes find it difficult to see things from the 'other person's' point of view.",
    IRI_Cognitive_5: "I try to look at things from the other person's point of view before making up my mind.",
    IRI_Cognitive_6: "When I'm upset at someone, I usually try to 'put myself in their shoes' for a while.",
    IRI_Cognitive_7: "Before criticising somebody, I try to imagine how I would feel if I were in their place.",
    IRI_Affective_1: "I often have tender, concerned feelings for people less fortunate than me.",
    IRI_Affective_2: "Sometimes I don't feel very sorry for other people when they are having problems.", 
    IRI_Affective_3: "When I see someone being taken advantage of, I feel kind of protective toward them.",
    IRI_Affective_4: "Other people's misfortunes do not usually disturb me a great deal.",
    IRI_Affective_5: "I am often quite touched by things that I see happen.",
    IRI_Affective_6: "I would describe myself as a pretty soft-hearted person.",
    IRI_Affective_7: "When I see someone being treated unfairly, I sometimes don't feel very much pity for them.",
}

function make_iri(items, required = true, ticks = ["Describes me well", "Does not describe me well"]) {
    items = shuffleObject(items)

    questions = []

    // Make questions
    for (const key of Object.keys(items)) {
        q = {
            title: items[key],
            name: key,
            type: "rating",
            displayMode: "buttons",
            // scaleColorMode: "colored",
            isRequired: required,
            minRateDescription: ticks[0],
            maxRateDescription: ticks[1],
            rateValues: [0, 1, 2, 3, 4, 5],
        }
        questions.push(q)
    }

    return { elements: questions }
}

const questionnaire_iri = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "About your feelings",
            description:
                "We are interested in your thoughts and feelings. Please read the statements below carefully and indicate the extent each statement describe you.",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: make_iri(items_iri),
        }
    },
    data: {
        screen: "questionnaire_iri",
    },
}


// SEACS ================================================

const items_seacs = {
    SEACS_SocialEffort_1: "I often arrange events with other people",
    SEACS_SocialConscientiousness_2: "I present myself in a way that makes a good impression on others",
    SEACS_SocialEffort_3: "When I have personal problems, I usually talk to friends or acquaintances about them",
    SEACS_SocialEffort_4: "When someone texts or emails me to say hello, I usually respond as soon as I can",
    SEACS_SocialEffort_5: "I make a lot of effort to connect with others",
    SEACS_SocialConscientiousness_6: "I compliment others when they have done something well",
    SEACS_SocialEffort_7: "I regularly message people I know on social media (e.g., Instagram)",
    SEACS_SocialEffort_8: "If I feel lonely, I find something to do with other people",
    SEACS_SocialConscientiousness_9: "I tend to ask other people how they are when I notice they're not feeling well",
    SEACS_SocialEffort_10: "I like to share my emotions with others",
    SEACS_SocialEffort_11: "I am often the one to call friends and/or family when I haven't spoken to them in a while",
    SEACS_SocialConscientiousness_12: "I usually try to help other people when they are feeling down",
}

function make_seacs(items, required = true, ticks = ["Strongly disagree", "Strongly agree"]) {
    items = shuffleObject(items)

    questions = []

    // Make questions
    for (const key of Object.keys(items)) {
        q = {
            title: items[key],
            name: key,
            type: "rating",
            displayMode: "buttons",
            // scaleColorMode: "colored",
            isRequired: required,
            minRateDescription: ticks[0],
            maxRateDescription: ticks[1],
            rateValues: [1, 2, 3, 4, 5, 6],
        }
        questions.push(q)
    }

    return { elements: questions }
}

const questionnaire_seacs = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "About your social interactions",
            description:
                "We are interested in your interactions with your social circle. Please read the statements below carefully and indicate the extent to which you agree with each statement.",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: make_seacs(items_seacs),
        }
    },
    data: {
        screen: "questionnaire_seacs",
    },
}

// Relationship Quality Scale (FQS/RQS) ================================================

const items_rqs = {
    RQS_1: "My friend/partner and I spend all our free time together",
    RQS_2: "If I have a problem at school, work or home, I can talk to my friend/partner about it",
    RQS_3: "If other people were bothering me, my friend/partner would help me",
    RQS_4: "My friend/partner thinks of fun things for us to do together",
    RQS_5: "My friend/partner helps me when I am having trouble with something",
    RQS_6: "If my friend/partner had to move away, I would miss them",
    RQS_7: "When I do a good job at something, my friend/partner is happy for me",
    RQS_8: "Sometimes my friend/partner does things for me, or makes me feel special",
    RQS_9: "Sometimes I quarrel, even violently, with my friend/partner",
    RQS_10: "My friend/partner would stick up for me if someone was causing me trouble",
    RQS_11: "My friend/partner can bug me or annoy me even though I ask them not to",
    RQS_12: "If I needed money my friend/partner would loan it to me",
    RQS_13: "After having fought, even violently, with my friend/partner, if I said sorry to them, I think that they would continue to be angry with me",
    RQS_14: "Sometimes my friend/partner and I just sit around and talk about things like study, work, and things we like",
    RQS_15: "My friend/partner would help me if I needed it",
    RQS_16: "If there is something bothering me, I can tell my friend/partner about it even if it is something I cannot tell to other people",
    RQS_17: "If my friend/partner or I do something that bothers the other one of us, we can make up easily",
    RQS_18: "My friend/partner and I can argue a lot",
    RQS_19: "My friend/partner and I disagree about many things",
    RQS_20: "If my friend/partner and I have a violent argument, we can say 'I'm sorry' and everything will be alright",
    RQS_21: "I feel happy when I am with my friend/partner",
    RQS_22: "I think about my friend/partner even when they are not around",
}

function make_rqs(items, required = true, ticks = ["Not at all true", "Absolutely true"]) {
    items = shuffleObject(items)

    questions = []

    // Make questions
    for (const key of Object.keys(items)) {
        q = {
            title: items[key],
            name: key,
            type: "rating",
            displayMode: "buttons",
            // scaleColorMode: "colored",
            isRequired: required,
            minRateDescription: ticks[0],
            maxRateDescription: ticks[1],
            rateValues: [1, 2, 3, 4, 5],
        }
        questions.push(q)
    }

    return { elements: questions }
}

const questionnaire_rqs = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "About your relationships",
            description:
                "Please read the statements below carefully and indicate the extent to which you agree with each statement.",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: make_rqs(items_rqs),
        }
    },
    data: {
        screen: "questionnaire_rqs",
    },
}