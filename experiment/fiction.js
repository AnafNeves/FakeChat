// Condition assignment
function shuffleArray(array) {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1))
        ;[array[i], array[j]] = [array[j], array[i]]
    }
    return array
}

// Random N integers between min and max (inclusive), sorted
function generateRandomNumbers(min, max, N) {
    return [...Array(max - min + 1).keys()]
        .map((i) => i + min)
        .sort(() => Math.random() - 0.5)
        .slice(0, N)
        .sort((a, b) => a - b)
}

// Trial counter for this task
var social_trialnumber = 1

var text_ticks_1to7_social = ["1", "2", "3", "4", "5", "6", "7"]

// VIGNETTE TEMPLATES ======================================================================

var vignettes = [
    {
        id: "romantic1",
        Topic: "Romantic",
        context_text:
            "The following screenshots are taken from a conversation posted on Reddit following a thread discussing people's experiences with romantic apps. Users have provided their consent for their images to be used during this study, provided any identifiable information is made anonymous.",
        intro_text:
            "You recently joined an online platform designed to help people seeking romantic relationships.",
        bubble_human: "(You are the bright pink speech bubble and your interaction partner is the dark purple speech bubble.)",
        bubble_AI: "(You are the pink speech bubble and your interaction partner is the white speech bubble.)",
        images_AI: ["101.png"],
        images_human: ["102.png"],
    },
    {
        id: "romantic2",
        Topic: "Romantic",
        context_text:
            "The following screenshots are taken from a conversation posted on Reddit following a thread discussing people's experiences with romantic apps. Users have provided their consent for their images to be used during this study, provided any identifiable information is made anonymous.",
        intro_text: 
            "You recently joined an online platform seeking advice for romantic relationships.",
        bubble_human: "(You are the teal blue speech bubble and your interaction partner is the navy speech bubble.)",
        bubble_AI: "(You are the dark grey speech bubble and your interaction partner is the black speech bubble.)",
        images_AI: ["201.png"],
        images_human: ["202.png"],
    },
    {
        id: "mental1",
        Topic: "Mental",
        context_text:
            "The following screenshots are taken from a conversation posted on Reddit following a thread discussing people's experiences with mental health support assistants. Users have provided their consent for their images to be used during this study, provided any identifiable information is made anonymous.",
        intro_text: 
            "You recently reached out to an online charity for support with your mental wellbeing.",
        bubble_human: "(You are the green speech bubble and your interaction partner is the white speech bubble.)",
        bubble_AI: "(You are the grey speech bubble and your interaction partner is the white speech bubble.)",
        images_AI: ["301.png"],
        images_human: ["302.png"],
    },
    {
        id: "mental2",
        Topic: "Mental",
        context_text:
            "The following screenshots are taken from a conversation posted on Reddit following a thread discussing people's experiences with mental health support assistants. Users have provided their consent for their images to be used during this study, provided any identifiable information is made anonymous.",
        intro_text: 
            "You recently engaged with an online platform designed to help support those struggling with their mental wellbeing.",
        bubble_human: "(You are the blue speech bubble and your interaction partner is the grey speech bubble.)",
        bubble_AI: "(You are the dark grey speech bubble and your interaction partner is the white speech bubble.)",
        images_AI: ["401.png"],
        images_human: ["402.png"],
    },
    {
        id: "trivial1",
        Topic: "Trivial",
        context_text:
            "The following screenshots are taken from a conversation posted on Reddit following a thread discussing people's experiences with travel planning assistants. Users have provided their consent for their images to be used during this study, provided any identifiable information is made anonymous.",
        intro_text: 
             "You are planning a holiday using an online-accessible travel assistant.",
        bubble_human: "(You are the orange speech bubble and your interaction partner is the white speech bubble.)",
        bubble_AI: "(You are the dark grey speech bubble and your interaction partner is the black speech bubble.)",
        images_AI: ["501.png"],
        images_human: ["502.png"],
    },
    {
        id: "trivial2",
        Topic: "Trivial",
        context_text:
            "The following screenshots are taken from a conversation posted on Reddit following a thread designed to help connect people to make new friends. It is designed for people to talk about their day-to-day experiences. Users have provided their consent for their images to be used during this study, provided any identifiable information is anonymised.",
        intro_text: 
             "You recently joined a cooking class and are trying to figure out what you should have for dinner.",
        bubble_human: "(You are the red speech bubble and your interaction partner is the white speech bubble.)",
        bubble_AI: "(You are the dark grey speech bubble and your interaction partner is the black speech bubble.)",
        images_AI: ["601.png"],
        images_human: ["602.png"],
    },
]

/// Condition assignment ============================================
function assignCondition(vignettes) {
    let new_list = []

    // Loop through unique Topic values
    for (let topic of [...new Set(vignettes.map((v) => v.Topic))]) {
        let topic_stims = vignettes.filter((v) => v.Topic === topic)
        shuffleArray(topic_stims)

        let conditions = ["Human", "AI"]
        let half = Math.floor(topic_stims.length / 2)

        let index = 0

        // assign evenly
        for (let c of conditions) {
            for (let i = 0; i < half; i++) {
                topic_stims[index].Condition = c
                index++
            }
        }

        // leftover (odd number)
        if (topic_stims.length % 2 !== 0) {
            topic_stims[index].Condition =
                conditions[Math.floor(Math.random() * 2)]
        }

        new_list.push(...topic_stims)
    }

    return shuffleArray(new_list)
}

// Function used to insert catch-trials ("what was the topic?") in some trials
function generateRandomNumbers(min, max, N) {
    return [...Array(max - min + 1).keys()]
        .map((i) => i + min)
        .sort(() => Math.random() - 0.5)
        .slice(0, N)
        .sort((a, b) => a - b) // Sort the numbers in ascending order
}

// Variables ===================================================================
var vignettes_trialnumber = 1
stimuli = assignCondition(vignettes)
for (let v of stimuli) {
    v.Stimulus = v.Condition === "AI" ? v.images_AI[0] : v.images_human[0]
}

// We make 2 catch trials (always starting from 2 = the first trial) - attention checks
catch_trials = [2].concat(generateRandomNumbers(1, vignettes.length, 2))
var task_preloadstims = {
    type: jsPsychPreload,
    images: vignettes.flatMap((v) => [
        ...v.images_AI.map((f) => "stimuli/" + f),
        ...v.images_human.map((f) => "stimuli/" + f),
    ]),
    message: "Loading stimuli...",
}

// ======================================================================
// PHASE 1
// ======================================================================
const phase1_instructions = {
    type: jsPsychSurvey,
    data: { screen: "phase1_instructions" },
    survey_json: {
        showQuestionNumbers: false,
        completeText: "Let's start",
        pages: [
            {
                elements: [
                    {
                        type: "html",
                        name: "Instructions1",
                        html: `
                            <div style="display: flex; justify-content: space-between; align-items: flex-start; gap: 20px;">
                            
                                <div style="flex: 2;">
                                    <h1 style="margin: 0;">Instructions</h1>
                                    <h2>Welcome</h2>

                                    <p>
                                        The following study is interested in looking at social interactions.
                                    </p>

                                    <p>
                                        Several screenshots of conversations have been taken from Reddit, 
                                        with the consent of users for their images to be used during this study, 
                                        provided any identifiable information is anonymised.
                                    </p>

                                    <p>
                                        <b>Please imagine that you are participating in the interaction while reading the dialogue.</b>
                                    </p>

                                    <p>
                                        After each interaction, you will be asked a few questions about your experience.
                                    </p>

                                    <p style="color: red; font-weight: bold;">
                                        If at any point you are uncomfortable, you can withdraw from the study by closing the tab.
                                    </p>
                                </div>

                                <div style="flex: 1; text-align: center;">
                                    <img src="stimuli/social_interaction.png" 
                                         alt="Social Interaction" 
                                         style="max-width: 100%; height: auto; border-radius: 10px;">
                                </div>

                            </div>
                        `,
                    },
                ],
            },
        ],
    },
}

// Give all vignettes a default flag
vignettes.forEach((v) => {
    v.HasAttentionCheck = false
})
// Randomly choose 2 distinct indices to get attention checks
var attention_indices = generateRandomNumbers(0, vignettes.length - 1, 2)
// Mark those
attention_indices.forEach((i) => {
    vignettes[i].HasAttentionCheck = true
})

// Show vignette
var phase1_vignette = {
    type: jsPsychHtmlButtonResponse,
    
    stimulus: function () {
        let context_text = jsPsych.evaluateTimelineVariable("context_text")
        let intro_text = jsPsych.evaluateTimelineVariable("intro_text")
        let bubble_human = jsPsych.evaluateTimelineVariable("bubble_human")
        let bubble_AI = jsPsych.evaluateTimelineVariable("bubble_AI")
        let condition = jsPsych.evaluateTimelineVariable("Condition")
        let img_file = jsPsych.evaluateTimelineVariable("Stimulus")

        let intro_img = img_file.replace(".png", "_intro.png")

        let bubble_text = condition === "Human"
            ? bubble_human
            : bubble_AI

        return `
            <div style="max-width: 900px; margin: 0 auto; text-align: left;">

                <p>${context_text}</p>

                <p>
                    <b>Imagine that this is your conversation.</b>
                    ${intro_text}
                    ${bubble_text}
                </p>

                <p style="color: red; font-weight: bold;">
                    If at any point you are uncomfortable with the content of the conversation, 
                    you can withdraw from the study by closing the tab.
                </p>

                <div style="margin-top: 20px; text-align: center;">
                    <img src="stimuli/${intro_img}" 
                         style="width:900px; max-width:100%; height:auto; margin-bottom: 20px;">
                </div>

                <div style="text-align: center;">
                    <img src="stimuli/${img_file}" 
                         style="width:550px; max-width:100%; height:auto;">
                </div>

                <div style="height: 30px;"></div>

                <div style="text-align: center;">
                    <p>You have reached the end of the conversation.</p>
                    <p>You will now be asked a sequence of questions regarding your interaction.</p>
                    <p>Remember to imagine this is you engaging in the conversation.</p>
                </div>

            </div>
        `
    },
    choices: ["Continue"],
    trial_duration: null,
    data: function () {
        return {
            screen: "vignette_image1",
            vignette_id: jsPsych.evaluateTimelineVariable("id"),
            condition: jsPsych.evaluateTimelineVariable("Condition"),
            topic: jsPsych.evaluateTimelineVariable("Topic"),
            stimulus: jsPsych.evaluateTimelineVariable("Stimulus"),
            trial_number: vignettes_trialnumber,
        }
    },
    
    on_finish: function () {
        vignettes_trialnumber += 1
    },
}

var vignette_scales_partner = [
    {
        title: "I can trust the interaction partner.",
        name: "PartnerTrust",
        type: "rating",
        displayMode: "buttons",
        rateCount: 7,
        rateMin: 1,
        rateMax: 7,
        minRateDescription: "Not at all",
        maxRateDescription: "Completely",
        isRequired: true,
    },
    {
        title: "The interaction partner seemed like a real social being rather than just text on a screen.",
        name: "PartnerReal",
        type: "rating",
        displayMode: "buttons",
        rateCount: 7,
        rateMin: 1,
        rateMax: 7,
        minRateDescription: "Not at all",
        maxRateDescription: "Completely",
        isRequired: true,
    },
]

var vignette_scales_conversation = [
    {
        title: "I was able to imagine myself in this conversation.",
        name: "ConversationMyself",
        type: "rating",
        displayMode: "buttons",
        rateCount: 7,
        rateMin: 1,
        rateMax: 7,
        minRateDescription: "Not at all",
        maxRateDescription: "Completely",
        isRequired: true,
    },
    {
        title: "If I were in this conversation, I would feel comfortable sharing personal information with this partner.",
        name: "ConversationSharing",
        type: "rating",
        displayMode: "buttons",
        rateCount: 7,
        rateMin: 1,
        rateMax: 7,
        minRateDescription: "Not at all",
        maxRateDescription: "Completely",
        isRequired: true,
    },
]

const vignette_scales = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            showQuestionNumbers: false,
            showNavigationButtons: true,
            goNextPageAutomatic: false,
            pages: [
                {
                    title:
                        "The following questions are interested in how you felt about your interaction partner.",
                    elements: vignette_scales_partner,
                },
                {
                    title:
                        "The following questions are interested in how you felt during the conversation.",
                    elements: vignette_scales_conversation,
                },
            ],
        }
    },
    data: {
        screen: "vignette_scales",
        attention_check: false,
    },
}

var vignette_attentioncheck = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            showQuestionNumbers: false,
            showNavigationButtons: true,
            goNextPageAutomatic: false,
            pages: [
                {
                    title:
                        "The following questions are interested in how you felt about your interaction partner.",
                    elements: vignette_scales_partner,
                },
                {
                    title:
                        "The following questions are interested in how you felt during the conversation.",
                    elements: vignette_scales_conversation.concat([
                        {
                            title: "What was the theme of the conversation?",
                            name: "AttentionCheck",
                            type: "radiogroup",
                            choices: [
                                "Romantic",
                                "Mental Health",
                                "Everyday / Trivial",
                                "I don't remember",
                            ],
                            showOtherItem: false,
                            isRequired: true,
                            colCount: 0,
                        },
                    ]),
                },
            ],
        }
    },
    data: {
        screen: "vignette_attentioncheck",
        attention_check: true,
    },
    on_finish: function (data) {
        var response = data.response || {}
        var answer = response.AttentionCheck
        var topic = jsPsych.evaluateTimelineVariable("Topic")
        var correct = false

        if (answer === "Romantic" && topic === "Romantic") {
            correct = true
        } else if (
            answer === "Mental Health" &&
            topic === "Mental"
        ) {
            correct = true
        } else if (
            answer === "Everyday / Trivial" &&
            topic === "Trivial"
        ) {
            correct = true
        }

        data.attention_correct = correct
    },
}

var t_vignette_attentioncheck = {
    timeline: [vignette_attentioncheck],
    conditional_function: function () {
        return jsPsych.evaluateTimelineVariable("HasAttentionCheck")
    },
}

var t_vignette_nocheck = {
    timeline: [vignette_scales],
    conditional_function: function () {
        return !jsPsych.evaluateTimelineVariable("HasAttentionCheck")
    },
}

// First-pass block: all 6 vignettes in this (shuffled) order
var task_phase1 = {
    timeline_variables: vignettes,
    timeline: [
        phase1_vignette,
        t_vignette_attentioncheck,
        t_vignette_nocheck,
    ],
    randomize_order: true,
}

// ======================================================================
// BLUFF - Phase 2
// ======================================================================

const phase2_instructions = {
    type: jsPsychSurvey,
    data: { screen: "phase2_instructions" },
    on_finish: function () {
        vignettes_trialnumber = 1 // Reset trial counter
    },
    survey_json: {
        showQuestionNumbers: false,
        completeText: "Start",
        pages: [
            {
                elements: [
                    {
                        type: "html",
                        name: "Instructions2",
                        html: `
                            <div style="display: flex; justify-content: space-between; align-items: flex-start; gap: 20px;">
                            
                                <div style="flex: 2;">
                                    <h1 style="margin: 0;">Real... or not?</h1>
                                    <h4>Thank you for staying with us so far!</h4>
                                    <p>There is <b>something important</b> we need to reveal... In the previous phase, you were shown conversations with interaction partners who were presented as AI chatbots or real people.</p>
                                    <p>We <b>randomly assigned the appearance of these conversations</b> to look like AI-human interactions or human-human interactions. Therefore, some conversations may not have been presented in their correct form.</p>
                                    <p>For example: an interaction presented to you in the form of a conversation with an artificial agent (e.g. ChatGPT) may have actually been an interaction between two humans.</p>
                                    <p>In this final phase, we want you to try to identify <b>the correct category</b> of each image. We will briefly present all the conversations once more, followed by one question about whether you think the interaction partner was a human or an artificial agent.</p>
                                    <p>Sometimes, it is hard to tell, but don't overthink it and <b>go with your gut feeling</b>. At the end, we will tell you if you were correct or wrong!</p>
                                </div>

                                <div style="flex: 1; text-align: center;">
                                    <img src="stimuli/example_vignette.png" 
                                        alt="Example" 
                                        style="max-width: 100%; height: auto; border-radius: 10px;">
                                </div>

                            </div>
                        `,
                    },
                ],
            },
        ],
    },
}

// Show image again, then confidence slider (AI vs Human)
var phase2_vignette = {
    // Switch to HTML plugin to allow custom Image -> Text -> Button layout
    type: jsPsychHtmlButtonResponse,
    
    stimulus: function () {
        // manually construct the image tag here
        var img_src = "stimuli/" + jsPsych.evaluateTimelineVariable("Stimulus");
        
        return `
            <div style="max-width: 900px; margin: 0 auto; text-align: center;">
                
                <img src="${img_src}" style="width:550px; max-width:100%; height:auto;">

                <div style="height: 20px;"></div>

                <p>
                    Based on this conversation, please rate how confident you are that the interaction partner was a human or an AI chatbot.
                </p>

            </div>
        `;
    },
    choices: ["Continue"],
    
    data: function () {
        return {
            screen: "phase2_vignette",
            vignette_id: jsPsych.evaluateTimelineVariable("id"),
            original_condition: jsPsych.evaluateTimelineVariable("Condition"),
            topic: jsPsych.evaluateTimelineVariable("Topic"),
            stimulus: jsPsych.evaluateTimelineVariable("Stimulus"),
        }
    },
}

var phase2_scale = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            goNextPageAutomatic: false,
            showQuestionNumbers: false,
            showNavigationButtons: true,
            pages: [
                {
                    elements: [
                        {
                            type: "html",
                            name: "Box",
                            html: `
                                <p class="responsive-box-for-survey"> </p>
                                <style>
                                .responsive-box-for-survey {
                                    width: 100%;
                                    height: 1px;
                                }
                                @media (min-width: 600px) {
                                    .responsive-box-for-survey {
                                    width: 600px;
                                    }
                                }
                                </style>`,
                        },
                        {
                            type: "slider",
                            name: "Belief_AI_Human",
                            title: "Based on this conversation, I believe the interaction partner is...",
                            description:
                                "Move the slider towards 'AI' or 'Human' to reflect your belief.",
                            isRequired: true,
                            min: -100,
                            max: 100,
                            step: 1,
                            customLabels: [
                                { value: -100, text: "AI" },
                                { value: 100, text: "Human" },
                            ],
                            validators: [
                                {
                                type: "expression",
                                expression: "{Belief_AI_Human} != 0",
                                text: "Please indicate a preference (choose a value other than 0)."
                                }
                            ]
                        },
                    ],
                },
            ],
        }
    },
    data: {
        screen: "phase2_scale",
    },
}

var task_phase2 = {
    timeline_variables: vignettes,
    timeline: [phase2_vignette, phase2_scale],
    randomize_order: true,
}

// ======================================================================
// PHASE 3 - Loneliness follow-up questions
// ======================================================================

const loneliness_general = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "Think back to a time where you needed help with a day to day task.",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: [
                {
                    elements: [
                        {
                            title: "In this situation, how likely would you be to carry out a conversation similar to the 'cooking class' and 'travel assistant' examples with a close friend?",
                            name: "TrivialFriend",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Not at all likely",
                            maxRateDescription: "Very likely",
                            isRequired: true,
                        },
                        {
                            title: "In this situation, how likely would you be to carry out a conversation similar to the 'cooking class' and 'travel assistant' examples with an AI chatbot (e.g. ChatGPT, Gemini, Siri)?",
                            name: "TrivialAI",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Not at all likely",
                            maxRateDescription: "Very likely",
                            isRequired: true,
                        },
                    ],
                },
            ],
        }
    },
    data: {
        screen: "loneliness_general",
    },
}

const loneliness_mental = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "Think back to a time when you felt the most alone when struggling mentally.",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: [
                {
                    elements: [
                        {
                            title: "In this situation, how likely would you be to carry out a conversation similar to the mental health examples with a close friend?",
                            name: "MentalFriend",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Not at all likely",
                            maxRateDescription: "Very likely",
                            isRequired: true,
                        },
                        {
                            title: "In this situation, how likely would you be to carry out a conversation similar to the mental health examples with an AI chatbot (e.g. ChatGPT, Gemini, Siri)?",
                            name: "MentalAI",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Not at all likely",
                            maxRateDescription: "Very likely",
                            isRequired: true,
                        },
                    ],
                },
            ],
        }
    },
    data: {
        screen: "loneliness_mental",
    },
}

const loneliness_romantic = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "Think back to a time when you felt the most alone when romantically unsatisfied.",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: [
                {
                    elements: [
                        {
                            title: "In this situation, how likely would you be to carry out a conversation similar to the romantic examples with a close friend?",
                            name: "RomanticFriend",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Not at all likely",
                            maxRateDescription: "Very likely",
                            isRequired: true,
                        },
                        {
                            title: "In this situation, how likely would you be to carry out a conversation similar to the romantic examples with an AI chatbot (e.g. ChatGPT, Gemini, Siri)?",
                            name: "RomanticAI",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Not at all likely",
                            maxRateDescription: "Very likely",
                            isRequired: true,
                        },
                    ],
                },
            ],
        }
    },
    data: {
        screen: "loneliness_romantic",
    },
}

const loneliness_people = {
    type: jsPsychSurvey,
    survey_json: function () {
        return {
            title: "Support outlets",
            showQuestionNumbers: false,
            goNextPageAutomatic: false,
            pages: [
                {
                    elements: [
                        {
                            title: "When I feel lonely, I turn to... (select all that apply)",
                            name: "LonelinessPeople",
                            type: "checkbox",
                            choices: [
                                "Friends",
                                "Family",
                                "AI (e.g. Siri, ChatGPT, Gemini, Alexa, Google Home, Replika, AI companion, etc.)",
                                "Favourite TV show/movie/YouTuber",
                                "Charities (e.g. Samaritans)",
                            ],
                            showOtherItem: true,
                            otherText: "Other",
                            otherPlaceholder: "Please specify",
                            showNoneItem: true,
                            isRequired: true,
                            colCount: 1,
                        },
                        {
                            visibleIf: "{LonelinessPeople} contains 'Friends'",
                            title: "I feel lonely when I engage with my friends.",
                            name: "LonelinessFriends",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Completely disagree",
                            maxRateDescription: "Completely agree",
                            isRequired: true,
                        },
                        {
                            visibleIf: "{LonelinessPeople} contains 'Family'",
                            title: "I feel lonely when I engage with my family.",
                            name: "LonelinessFamily",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Completely disagree",
                            maxRateDescription: "Completely agree",
                            isRequired: true,
                        },
                        {
                            visibleIf: "{LonelinessPeople} contains 'AI (e.g. Siri, ChatGPT, Gemini, Alexa, Google Home, Replika, AI companion, etc.)'",
                            title: "I feel lonely when I engage with AI.",
                            name: "LonelinessAI",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Completely disagree",
                            maxRateDescription: "Completely agree",
                            isRequired: true,
                        },
                        {
                            visibleIf: "{LonelinessPeople} contains 'Favourite TV show/movie/YouTuber'",
                            title: "I feel lonely when I engage with my favourite TV show/movie/YouTuber.",
                            name: "LonelinessMedia",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Completely disagree",
                            maxRateDescription: "Completely agree",
                            isRequired: true,
                        },
                        {
                            visibleIf: "{LonelinessPeople} contains 'Charities (e.g. Samaritans)'",
                            title: "I feel lonely when I engage with charities (e.g. Samaritans).",
                            name: "LonelinessCharities",
                            type: "rating",
                            displayMode: "buttons",
                            rateCount: 7,
                            rateMin: 1,
                            rateMax: 7,
                            minRateDescription: "Completely disagree",
                            maxRateDescription: "Completely agree",
                            isRequired: true,
                        },
                    ],
                },
            ],
        }
    },
    data: {
        screen: "loneliness_people",
    },
}